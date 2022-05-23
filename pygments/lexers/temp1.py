class FlexLexer(RegexLexer):
    """
    Lexer for `Flex <http://github.com/westes/flex>`_,

    .. versionadded:: 2.13
    """

    name = 'Flex'
    aliases = ['flex']
    filenames = ['*.lex', '*.l']
    mimetypes = ['text/x-lex']
    url = 'http://github.com/westes/flex'

    tokens = {
        # Handle the definitions section
        'root': [
            (r'(%{|%})', String.Delimiter),
            (r'%%', String.Delimiter, 'rules'),
            (r'(#include)(\s)(<\w*\.*\w*>)', bygroups(Keyword, Whitespace, Name)),
            (r'(#define)(\s)(\w*\.*\w*)', bygroups(Keyword, Whitespace, Name)),
            (words(('bool', 'int', 'long', 'float', 'short', 'double', 'char',
                'unsigned', 'signed', 'void'), suffix=r'\b'), Keyword.Type),
            (r'\n', Whitespace),
            (r'\s+', Whitespace),
            (r'\\\n', Text),  # line continuation
            include('whitespace'),
            (r'[|/*+?^$.\-=]', Operator),
            (r'(%[sx])(\s)(\w+)', bygroups(Keyword, Whitespace, Name)),
            (r'((?!\d)(?:[\w$]|\\u[0-9a-fA-F]{4}|\\U[0-9a-fA-F]{8})+)(\s+)(=)', bygroups(Name.Variable, Whitespace, Operator)),
            (r'(\b[_a-zA-Z0-9][\w\-]+)(\s+)', bygroups(Name, Whitespace), 'regex'),
        ],

        # Handle the rules section
        'rules': [
            include('whitespace'),
            (r'%%', String.Delimiter, ('usercode', '#pop')),
        ],
        # Handle the user code section
        'usercode': [
            (r'(.+?)', using(CLexer)),
            include('whitespace'),
        ],
        # Handle whitespace
        'whitespace': [
            (r'%[{}]', Comment.Preproc),
            (r'\s+', Whitespace),
            (r'(/\*)(.)+(\*/)', Comment.Single),
        ],
        # Handle regular expressions
        'regex': [
            (r'\n', Whitespace, '#pop'),
            (r'"', String, 'string'),
            (r'\[:(alnum|alpha|blank|cntrl|digit|graph|lower|print|punct|space|upper|xdigit):\]', Name.Builtin),
            (r'\{[\w_]+\}', Name.Variable),
            (r'\d+', Number),
            (r'\w', Name),
            (r'[|/*+?^$.-]', Operator),
            (r'\\[abfnrtv]', String.Esacpe),
            (r'\\[AbBdDsSwWZ]', String.Regex),
            (r'\\.', String.Literal),
            (r'[\[\]{},\(\)]', Punctuation),
        ],
        # Handle strings
        'string': [
            (r'"', String, '#pop'),
            (r'\\[abfnrtv]', String.Esacpe),
            (r'\\.', String.Literal),
            (r'.', String),
        ],
    }


    # tokens = {
    #     'keywords': [
    #         (words((
    #             'yytext','yylmax','yyleng','yyin','yylineno','yy_current_buffer', 'yy_break','yy_buffer_state','yy_flex_debug','yy_end_of_buffer_char','yy_user_action','yy_user_init','yy_act','yy_interactive','yy_start','yy_decl','yy_flex_major_version', 'yy_flex_minor_version',
    #         )), Name.Builtin),
    #         (words((
    #             'yylex', 'yymore', 'yyterminate', 'yy_input', 'yyless', 'yycopy', 'yyrestart', 'yy_scan_string', 'yy_scan_bytes', 'yy_scan_buffer', 'yywrap',  'yy_set_interactive','yy_set_bol','yy_at_bol','yy_create_buffer','yy_delete_buffer','yy_flush_buffer', 'yy_flex_debug','yy_init_buffer','yy_flush_buffer','yy_load_buffer_state','yy_switch_to_buffer','yy_pop_state','yy_push_state','yy_top_state','yyFlexLexer'
    #         )), Name.Function),
    #         inherit,
    #     ],
    #     'statements': [
    #         (words((
    #             'if','then','begin','end','procedure','function'), suffix=r'\b'),
    #          Keyword),
    #         (words(('nx_struct', 'nx_union', 'nx_int8_t', 'nx_int16_t', 'nx_int32_t',
    #                 'nx_int64_t', 'nx_uint8_t', 'nx_uint16_t', 'nx_uint32_t',
    #                 'nx_uint64_t'), suffix=r'\b'),
    #          Keyword.Type),
    #         inherit,
    #     ]
    # }