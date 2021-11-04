// STD Dependencies -----------------------------------------------------------
use std::io::prelude::*;
use std::sync::{Arc, Mutex};
use std::sync::atomic::AtomicBool;
use std::time::{Duration, Instant};
use std::net::{SocketAddr, TcpStream};
use std::collections::{HashMap, VecDeque};


// External Dependencies ------------------------------------------------------
use serde::Deserialize;
use tower_lsp::Client;
use tower_lsp::lsp_types::request::WorkDoneProgressCreate;
use tower_lsp::lsp_types::{NumberOrString, WorkDoneProgressCreateParams};


// Internal Dependencies ------------------------------------------------------
use crate::types::{InlayHintsNotification, InlayHintsParams};


// Types ----------------------------------------------------------------------
#[derive(Debug, Deserialize, Clone)]
pub struct EmulatorStatus {
    emulator: String,
    debugger: u8,
    paused: u8,
    menu: u8,
    filename: String,
    crc: u32,
    title: String
}

#[derive(Debug, Deserialize)]
pub struct EmulatorAddressValue {
    address: u16,
    value: u8
}


// Emulator Communication -----------------------------------------------------
pub async fn watch(
    client: Arc<Client>,
    emulator: Arc<AtomicBool>,
    shutdown: Arc<AtomicBool>,
    queries: Arc<Mutex<VecDeque<u16>>>,
    results: Arc<Mutex<HashMap<u16, u8>>>
) {
    let addr: SocketAddr = "127.0.0.1:8765".parse().unwrap();
    loop {
        if let Ok(stream) = TcpStream::connect_timeout(&addr, Duration::from_millis(10)) {
            log::info!("Emulator attached");
            emulator.store(true, std::sync::atomic::Ordering::SeqCst);
            handle(client.clone(), stream, shutdown.clone(), queries.clone(), results.clone()).await;
            emulator.store(false, std::sync::atomic::Ordering::SeqCst);
            log::info!("Emulator detached");
        }
        tokio::time::sleep(tokio::time::Duration::from_millis(1000)).await;
    }
}

async fn handle(
    client: Arc<Client>,
    stream: TcpStream,
    shutdown: Arc<AtomicBool>,
    queries: Arc<Mutex<VecDeque<u16>>>,
    results: Arc<Mutex<HashMap<u16, u8>>>
) {
    communicate(client, stream, shutdown, queries, move |msg| {
        if let Ok(status) = serde_json::from_str::<EmulatorStatus>(msg) {
            Some(format!("{} via {} [{}]", status.title, status.emulator, if status.debugger == 1 {
                "DEBUGGER"

            } else if status.menu == 1 {
                "MENU"

            } else if status.paused == 1 {
                "PAUSED"

            } else {
                "RUNNING"
            }))

        } else if let Ok(value) = serde_json::from_str::<EmulatorAddressValue>(msg) {
            if let Ok(mut r) = results.lock() {
                r.insert(value.address, value.value);
            }
            None

        } else {
            log::info!(&format!("Unknown Emulator Message: {:?}", msg));
            None
        }

    }).await;
}

async fn communicate<C: FnMut(&str) -> Option<String>>(
    client: Arc<Client>,
    mut stream: TcpStream,
    shutdown: Arc<AtomicBool>,
    queries: Arc<Mutex<VecDeque<u16>>>,
    mut message: C
) {
    // Setup Stream
    stream.set_nodelay(true).ok();
    stream.set_read_timeout(Some(Duration::from_millis(25))).ok();
    stream.write_all(&[0x00, 0xE0]).ok();

    // Report Progress to Editor
    let token = NumberOrString::String("emulation".to_string());
    client.send_custom_request::<WorkDoneProgressCreate>(WorkDoneProgressCreateParams {
        token: token.clone()

    }).await.ok();
    client.show_progress_begin(token.clone(), "Emulating", "Connecting...").await;

    // Send and Receive Data from Emulator
    let mut status_timer = Instant::now();
    let mut buffer: Vec<u8> = Vec::new();
    let mut received = [0; 1024];
    let mut update_hints = Instant::now();
    loop {
        // Close connection if LSP is shutting down
        if shutdown.load(std::sync::atomic::Ordering::SeqCst) {
            break;
        }

        // Handle emulator queries
        if let Ok(mut q) = queries.try_lock() {
            while let Some(address) = q.pop_front() {
                stream.write_all(&[address as u8, (address >> 8) as u8]).ok();
            }
        }

        // Query emulator status
        if status_timer.elapsed() > Duration::from_millis(250) {
            stream.write_all(&[0x00, 0xE0]).ok();
            status_timer = Instant::now();
        }

        // Send updated hints to client in a regular fashion
        if update_hints.elapsed() > Duration::from_millis(5000) {
            client.send_custom_notification::<InlayHintsNotification>(InlayHintsParams {}).await;
            update_hints = Instant::now();
        }

        // TODO exit if no status is received for 1000ms
        match stream.read(&mut received) {
            Ok(0) => break,
            Ok(n) => {
                for i in &received[0..n] {
                    buffer.push(*i);
                    if *i == '}' as u8 {
                        let part = std::mem::replace(&mut buffer, Vec::new());
                        if let Ok(json) = String::from_utf8(part) {
                            if let Some(message) = message(json.trim()) {
                                client.show_progress_report(token.clone(), message).await;
                            }
                        }
                    }
                }
            },
            Err(e) => if e.kind() != std::io::ErrorKind::WouldBlock {
                log::warn!(&format!("Emulator Error: {:?}", e.kind()));
                break;
            }
        }
    }
    client.show_progress_end(token, "Emulating stopped").await;
}

