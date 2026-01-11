// Tree-sitter grammar for the Reality language
// This grammar is based on the Haskell megaparsec parser found in
// `src/Language/Reality/Frontend/Parser/*.hs` and attempts to model the same
// top-level constructs, expressions, statements, types and lexing rules.
//
// Note: Tree-sitter grammars cannot fully emulate Megaparsec behavior (for
// example, nested block comments or every parsing-time check), but this grammar
// aims to be compatible and useful for syntax highlighting, basic structural
// parsing and IDE features.

module.exports = grammar({
    name: 'reality',

    word: $ => $.identifier,

    extras: $ => [
        $.comment,
        /\s+/
    ],

    conflicts: $ => [
        [$.expression, $.lambda_expression],
        [$.annotation_name_with_optional_type, $.pattern_variable],
        [$.parenthesized_expression, $.tuple_expression]
    ],

    rules: {
        // Top-level program: many top-level declarations, allow optional semicolons
        source_file: $ => seq(
            optional(repeat(seq($.top_level, optional($.semi))))
        ),

        semi: $ => ';',

        // COMMENTS ---------------------------------------------------------------
        comment: $ => choice(
            token(seq('//', /.*/)),
            // Non-nestable block comment (Tree-sitter cannot easily nest)
            token(seq("/*", /[^*]*\*+([^/*][^*]*\*+)*\//))
        ),

        // TOP-LEVEL NODES -------------------------------------------------------
        top_level: $ => choice(
            $.public_declaration,
            $.constant_declaration,
            $.function_declaration,
            $.type_alias,
            $.import_declaration,
            $.module_declaration,
            $.structure_declaration,
            $.external_function,
            $.extern_let,
            $.property_declaration,
            $.implementation_declaration,
            $.annotation_top,
            $.enumeration_declaration
        ),

        constant_declaration: $ => seq(
            'const',
            $.annotation_name_with_type, // name : type
            '=',
            $.expression
        ),

        function_declaration: $ => seq(
            'fn',
            $.identifier,
            optional($.generic_params),
            $.param_list, // ( params... )
            '->',
            $.type,
            $.block_expression
        ),

        type_alias: $ => seq(
            'type',
            $.identifier,
            optional($.generic_params),
            '=',
            $.type
        ),

        import_declaration: $ => seq(
            'import',
            $.module_path
        ),

        public_declaration: $ => seq(
            'pub',
            $.top_level
        ),

        module_declaration: $ => seq(
            'mod',
            $.identifier,
            $.block_many_toplevel
        ),

        structure_declaration: $ => seq(
            'struct',
            $.identifier,
            optional($.generic_params),
            $.struct_fields_block
        ),

        external_function: $ => seq(
            'extern',
            'fn',
            $.identifier,
            optional($.generic_params),
            $.param_list,
            '->',
            $.type
        ),

        extern_let: $ => seq(
            'extern',
            'let',
            $.identifier,
            ':',
            $.type
        ),

        property_declaration: $ => seq(
            'property',
            $.identifier,
            optional($.generic_params),
            $.param_list,
            '->',
            $.type
        ),

        implementation_declaration: $ => seq(
            'impl',
            'fn',
            $.impl_for, // (name: Type)
            $.identifier,
            optional($.generic_params),
            $.param_list,
            '->',
            $.type,
            $.block_expression
        ),

        annotation_top: $ => seq(
            '#',
            '[',
            optional($.annotation_args),
            ']',
            choice($.block_many_toplevel, $.top_level)
        ),

        enumeration_declaration: $ => seq(
            'enum',
            $.identifier,
            optional($.generic_params),
            $.enum_variants_block
        ),

        // Helpers for blocks that contain many top-level entries
        block_many_toplevel: $ => seq(
            '{',
            repeat(seq($.top_level, optional($.semi))),
            '}'
        ),

        struct_fields_block: $ => seq(
            '{',
            optional(seq($.struct_field, repeat(seq(',', $.struct_field)), optional(','))),
            '}'
        ),

        struct_field: $ => seq($.identifier, ':', $.type),

        enum_variants_block: $ => seq(
            '{',
            optional(seq($.enum_variant, repeat(seq(',', $.enum_variant)), optional(','))),
            '}'
        ),

        enum_variant: $ => seq(
            $.identifier,
            optional(seq('(', commaSep($.type), ')'))
        ),

        impl_for: $ => seq('(', $.identifier, ':', $.type, ')'),

        annotation_args: $ => commaSep1($.expression),

        // MODULE / PATH ---------------------------------------------------------
        module_path: $ => seq(
            $.identifier,
            repeat(seq('.', $.identifier))
        ),

        // GENERICS / PARAMS ----------------------------------------------------
        generic_params: $ => seq('<', commaSep1($.identifier), '>'),

        param_list: $ => seq('(', optional(commaSep($.param)), ')'),

        param: $ => $.annotation_name_with_type, // name : type

        annotation_name_with_type: $ => seq($.identifier, ':', $.type),

        // TYPES ----------------------------------------------------------------
        // This is a simplified but practical type grammar that matches typical
        // patterns used by the Haskell parser reference.
        type: $ => choice(
            $.simple_type,
            $.generic_type,
            $.tuple_type,
            $.pointer_like_type,
            $.function_type
        ),

        simple_type: $ => $.identifier,

        generic_type: $ => seq($.identifier, $.type_arguments),

        type_arguments: $ => seq('<', commaSep1($.type), '>'),

        tuple_type: $ => seq('(', optional(seq($.type, repeat(seq(',', $.type)))), ')'),

        pointer_like_type: $ => seq(choice('*', '&'), $.type),

        function_type: $ => prec.right(seq("fn", "(", commaSep($.type), ")", "->", $.type)),

        // EXPRESSIONS ----------------------------------------------------------
        expression: $ => choice(
            $.literal,
            $.block_expression,
            $.let_in_expression,
            $.lambda_expression,
            $.if_expression,
            $.new_expression,
            $.struct_creation_expression,
            $.size_of_expression,
            $.for_in_statement, // top-level style for/statement usage
            $.while_statement,
            $.return_statement,
            $.break_statement,
            $.continue_statement,
            $.binary_expression,
            $.unary_expression,
            $.call_expression,
            $.index_expression,
            $.field_access,
            $.tuple_expression,
            $.list_expression,
            $.variable_expression,
            $.parenthesized_expression
        ),

        block_expression: $ => seq(
            '{',
            optional(repeat(seq($.statement, optional($.semi)))),
            '}'
        ),

        // LET-IN: 'let' name(: type)? = expr 'in' expr
        let_in_expression: $ => seq(
            'let',
            $.annotation_name_with_optional_type,
            '=',
            $.expression,
            'in',
            $.expression
        ),

        annotation_name_with_optional_type: $ => seq($.identifier, optional(seq(':', $.type))),

        // LAMBDA: | params | -> type? body
        lambda_expression: $ => seq(
            '|',
            optional(commaSep($.lambda_param)),
            '|',
            choice(
                seq('->', $.type, $.block_expression),
                $.block_expression,
                $.expression
            )
        ),

        lambda_param: $ => choice(
            $.annotation_name_with_optional_type,
            $.pattern // patterns allowed in lambda arguments in Haskell parser (converted to temp symbol)
        ),

        // TERNARY / IF expressions (if cond { then } else { else })
        if_expression: $ => seq(
            'if',
            $.expression,
            $.block_expression,
            optional(seq('else', $.expression))
        ),

        new_expression: $ => seq('new', $.expression),

        struct_creation_expression: $ => seq(
            'struct',
            $.type,
            '{',
            optional(seq($.struct_assignment_field, repeat(seq(',', $.struct_assignment_field)), optional(','))),
            '}'
        ),

        struct_assignment_field: $ => seq($.identifier, ':', $.expression),

        size_of_expression: $ => seq('sizeof', '(', $.type, ')'),

        // STATEMENTS (some expressions act as statements too)
        statement: $ => choice(
            $.expression,
            $.let_statement
        ),

        // let <ident>(: type)? = expr
        let_statement: $ => seq('let', $.annotation_name_with_optional_type, '=', $.expression),

        for_in_statement: $ => seq('for', $.identifier, 'in', $.expression, $.block_expression),

        while_statement: $ => seq('while', $.expression, $.block_expression),

        return_statement: $ => seq('return', $.expression),

        break_statement: $ => 'break',
        continue_statement: $ => 'continue',

        // PATTERNS -------------------------------------------------------------
        pattern: $ => choice(
            $.pattern_variable,
            $.pattern_wildcard,
            $.pattern_literal,
            $.pattern_constructor,
            $.pattern_structure,
            $.pattern_parens,
            $.pattern_let
        ),

        pattern_variable: $ => $.identifier,
        pattern_wildcard: $ => '_',
        pattern_literal: $ => $.literal,
        pattern_constructor: $ => seq($.identifier, '(', optional(seq($.pattern, repeat(seq(',', $.pattern)), optional(','))), ')'),
        pattern_structure: $ => seq('struct', $.type, '{', optional(seq($.pattern_field, repeat(seq(',', $.pattern_field)), optional(','))), '}'),
        pattern_field: $ => seq($.identifier, optional(seq(':', $.pattern))),
        pattern_parens: $ => seq('(', optional(seq($.pattern, repeat(seq(',', $.pattern)), optional(','))), ')'),
        pattern_let: $ => seq('let', $.identifier),

        // PRIMARY FORMS --------------------------------------------------------
        parenthesized_expression: $ => seq('(', $.expression, ')'),

        tuple_expression: $ => seq('(', optional(seq($.expression, repeat(seq(',', $.expression)), optional(','))), ')'),

        list_expression: $ => seq('[', optional(commaSep($.expression)), ']'),

        literal: $ => choice(
            $.number,
            $.string,
            $.interpolated_string,
            $.boolean,
            $.char
        ),

        number: $ => /\d+/,

        string: $ => token(seq('"', repeat(choice(/[^"\\]+/, /\\./)), '"')),

        // f" ... { expr } ... " style interpolation similar to Haskell parser
        interpolated_string: $ => seq(
            'f"',
            repeat(choice(
                /[^"{\\]+/,
                /\\./,
                seq('{', $.expression, '}')
            )),
            '"'
        ),

        boolean: $ => choice('true', 'false'),
        char: $ => token(seq("'", /[^'\\]/, "'")),

        // IDENTIFIERS & OPERATORS ----------------------------------------------
        // Distinguish two identifier flavors used in the Haskell source:
        // - `identifier` which is lexeme-separated and cannot be reserved keywords
        // - `non_lexed_id` used for field names, module path segments (does not consume trailing whitespace)
        identifier: $ => token(seq(/[A-Za-z_][A-Za-z0-9_]*/)),

        variable_expression: $ => $.identifier,

        // INDEXING, CALLS, FIELD ACCESS (chained postfix)
        call_expression: $ => prec.left(10, seq($.expression, $.arguments)),

        arguments: $ => seq('(', optional(commaSep($.expression)), ')'),

        index_expression: $ => prec.left(20, seq($.expression, '[', $.expression, ']')),

        field_access: $ => prec.left(30, seq($.expression, choice('.', '->'), $.identifier, optional($.type_arguments), optional($.arguments))),

        // UNARY / POSTFIX ------------------------------------------------------
        unary_expression: $ => choice(
            seq('*', $.expression),
            seq('&', $.expression),
            seq('!', $.expression),
            seq('-', $.expression)
        ),

        // postfix 'as' type and 'is' pattern
        postfix_as: $ => seq($.expression, 'as', $.type),
        postfix_is: $ => seq($.expression, 'is', $.pattern),

        // BINARY OPERATORS with precedence roughly matching Haskell parser
        binary_expression: $ => choice(
            // Exponentiation (left associative)
            prec.left(1, seq($.expression, '**', $.expression)),

            // Multiplicative
            prec.left(2, seq($.expression, choice('*', '/', '%'), $.expression)),

            // Additive
            prec.left(3, seq($.expression, choice('+', '-'), $.expression)),

            // Relational
            prec.left(4, seq($.expression, choice('>=', '<=', '>', '<'), $.expression)),

            // Equality
            prec.left(5, seq($.expression, choice('==', '!='), $.expression)),

            // Logical AND
            prec.left(6, seq($.expression, '&&', $.expression)),

            // Logical OR
            prec.left(7, seq($.expression, '||', $.expression)),

            // Assignment (right associative)
            prec.right(80, seq($.expression, '=', $.expression))
        ),

        // VARIABLE / IDENTIFIER TYPES ------------------------------------------
        // Non-lexed operators as fields are allowed in Haskell parser, but for tree-sitter
        // we keep operators as punctuation tokens. Users can extend this as needed.

        // NON-TERMINAL HELPERS -------------------------------------------------
        module_identifier: $ => seq($.identifier, repeat(seq('.', $.identifier))),

        // OPERATORS & PUNCTUATION (tokens are produced inline in rules above) --
        // We rely on literal strings wherever possible. If you need operator-as-token
        // names, add them here as extras.

    }
});

// Helpers used inside the grammar definition (local JS helpers)
function commaSep(rule) {
    return optional(seq(rule, repeat(seq(',', rule))));
}
function commaSep1(rule) {
    return seq(rule, repeat(seq(',', rule)));
}
