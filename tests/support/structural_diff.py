import html.parser


class Parser(html.parser.HTMLParser):
    def __init__(self):
        super().__init__()
        self._stream = []

    def handle_starttag(self, tag, attrs):
        attrs = sorted(attrs, key=lambda x: x[0])
        attrs = '|'.join([k[0] + ':' + k[1] for k in attrs])
        self._stream.append(('<', tag, attrs))

    def handle_endtag(self, tag):
        self._stream.append(('>', tag, ''))

    def handle_data(self, data):
        self._stream.append(('_', data, ''))

    @property
    def stream(self):
        return self._stream


def _serialize(t):
    parser = Parser()
    parser.feed(t)
    return parser.stream


def structural_diff(a, b):
    """Check if there is a structural difference between two HTML files."""
    a_s = _serialize(a)
    b_s = _serialize(b)

    for e, f in zip(a_s, b_s):
        assert e == f, f'Expected: {e}, found: {f}'
