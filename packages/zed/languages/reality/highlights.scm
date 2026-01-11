; Basic Tree-sitter highlight queries for the Reality language
; These queries provide a practical, minimal set of captures for syntax highlighting
; using the grammar defined in packages/tree-sitter/grammar.js.
;
; Notes:
; - We capture node types (identifiers, literals, declarations, types, keywords, etc.)
;   using conventional highlight group names (e.g. @keyword, @function, @type).
; - This is intentionally conservative: it aims to give good highlighting without
;   relying on fragile token-level patterns. Editors can extend these queries later.

; COMMENTS -----------------------------------------------------------------
((comment) @comment)

; LITERALS ----------------------------------------------------------------
((number) @number)
((string) @string)
((interpolated_string) @string)
((boolean) @boolean)
((char) @character)

; Interpolated string: highlight embedded expressions
((interpolated_string (expression) @subst))

; KEYWORDS & DECLARATIONS -------------------------------------------------
; Top-level declaration keywords
((constant_declaration "const") @keyword)
((function_declaration "fn") @keyword)
((type_alias "type") @keyword)
((import_declaration "import") @keyword)
((public_declaration "pub") @keyword)
((module_declaration "mod") @keyword)
((structure_declaration "struct") @keyword)
((external_function "extern") @keyword)
((extern_let "extern") @keyword)
((property_declaration "property") @keyword)
((implementation_declaration "impl") @keyword)
((enumeration_declaration "enum") @keyword)

; Control keywords / statements
((if_expression "if") @keyword.control)
((if_expression "else") @keyword.control)
((for_in_statement "for") @keyword.control)
((for_in_statement "in") @keyword.control)
((while_statement "while") @keyword.control)
((return_statement "return") @keyword.control)
((break_statement) @keyword.control)
((continue_statement) @keyword.control)
((let_statement "let") @keyword)

; Other keywords
((new_expression "new") @keyword)
((size_of_expression "sizeof") @keyword)

; ANNOTATIONS --------------------------------------------------------------
((annotation_top) @annotation)
; capture annotation args (identifier, literals) when present
((annotation_top (annotation_args (expression) @annotation) ) )

; IDENTIFIERS: functions, constants, types, variables ---------------------
; Function names (top-level and external)
((function_declaration (identifier) @function)
 (1))
((external_function (identifier) @function)
 (1))
((implementation_declaration (identifier) @function)
 (1))

; Constant names
((constant_declaration (annotation_name_with_type (identifier) @constant))
 (1))
; Fallback: any identifier direct child of constant_declaration
((constant_declaration (identifier) @constant)
 (1))

; Type names (structs, enums, simple_type, generic_type)
((structure_declaration (identifier) @type)
 (1))
((enumeration_declaration (identifier) @type)
 (1))
((simple_type (identifier) @type))
((generic_type (identifier) @type))
((type_arguments (type) @type))

; Module / import names
((import_declaration (module_path (identifier) @namespace)))
((module_declaration (identifier) @namespace))

; Variables and identifiers in expressions
((variable_expression) @variable)
((annotation_name_with_optional_type (identifier) @variable)) ; local let / lambda names
((annotation_name_with_type (identifier) @variable)) ; params with explicit types

; Parameters (function params, lambda params)
((param (identifier) @parameter))
((lambda_param (identifier) @parameter))

; PATTERNS & CONSTRUCTORS -------------------------------------------------
((pattern_constructor (identifier) @constructor))
((enum_variant (identifier) @constructor))
((pattern_structure (identifier) @type))

; STRUCT / FIELD NAMES ---------------------------------------------------
((struct_field (identifier) @field))
((struct_assignment_field (identifier) @field))
((pattern_field (identifier) @field))
((field_access (identifier) @property)) ; x.foo or x->foo
((struct_creation_expression (struct_assignment_field (identifier) @field)))

; CALLS / INDEXING / POSTFIX ----------------------------------------------
((call_expression) @call)
((index_expression) @index)
((field_access) @property)

; TYPES & TYPE-LIKE FORMS -------------------------------------------------
((function_type "fn") @keyword)
((pointer_like_type) @type)
((tuple_type) @type)

; GENERICS ----------------------------------------------------------------
((generic_params (identifier) @type.parameter))
((type_arguments (type) @type))

; OPERATORS / BINARY / UNARY ----------------------------------------------
; Highlight whole expressions so operators are visible (editor may still color tokens)
((binary_expression) @operator)
((unary_expression) @operator)
((postfix_as) @operator)
((postfix_is) @operator)

; PUNCTUATION / BRACES / PARENTHESIS -------------------------------------
((block_expression) @punctuation.bracket)
((block_many_toplevel) @punctuation.bracket)
((arguments) @punctuation)
((param_list) @punctuation)
((tuple_expression) @punctuation)
((list_expression) @punctuation)

; PATTERN HIGHLIGHTING ----------------------------------------------------
((pattern_variable) @variable)
((pattern_wildcard) @constant)
((pattern_literal) @constant)

; MISC --------------------------------------------------------------------
; Module paths and dotted identifiers (segments as namespaces)
((module_path (identifier) @namespace))

; Provide a generic fallback for any identifier child inside interesting nodes
; (This helps highlight names that weren't caught above)
((identifier) @identifier)

; End of file. Add more specific captures later as needed.
