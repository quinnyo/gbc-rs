// STD Dependencies -----------------------------------------------------------



// Macro Level Lexer Implementation -------------------------------------------
pub struct MacroLexer {

    // TODO go through all tokens and find and store macro definitions
    // TODO until no more macro calls have been expanded
        // TODO expand macro calls by...
            // TODO creating a copy of the macro tokens
            // TODO replacing all argument references with the supplied arguments tokens and
                // flattening groups
            // TODO inserting the expanded macro tokens into the call site

    // TODO mark all inner tokens as being expanded

}

pub struct ValueLexer {
    // TODO Parse Numbers
    // TODO Combine Label Defs and Refs
    // TODO Combine Operators
}

pub struct ExpressionLexer {
    // TODO 1. Combine all Expressions and replace their tokens with one new tokens
    // TODO 2. Start parsing the Tokens from start to end (instructions, directives)
    // TODO 3. Create sections slices and assign parsed entries to their section indicies
    // TODO 4. Calculate sizes of all entries

    // TODO 1. Go through all sections ordered by base adress ascending
        // TODO 1.1. Calculate offsets for all the section's entries
        // TODO 1.2. Check for section overlaps / out of bounds

    // TODO 2. Peform jump target resolution
    // TODO 3. Perform instruction optimizations
    // TODO 4. go back to 1 and repeat until no more optimizations can be applied

    // TODO 1. Go through all sections ordered by base adress ascending
    // TODO 2. Serialize all section entries into the corresponding ROM part
}

// pub enum Entry {
//     Variable, // DB DS DW
//
// }
