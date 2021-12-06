import lxml.html
import lxml.etree
import pytest

def _serialize(t):
    for a, e in lxml.etree.iterwalk(t, events=("start", "end"),):
        text = e.text.strip() if e.text else ""
        yield (a, e.tag, repr(text), ', '.join([k[0]+':'+k[1] for k in sorted(e.attrib.items(), key = lambda x: x[0])]))

def structural_diff(a, b):
    """Check if there is a structural difference between two HTML files."""
    a_s = _serialize(lxml.html.fromstring(a))
    b_s = _serialize(lxml.html.fromstring(b))
    
    for e, f in zip(a_s, b_s):
        print(e, f)
        assert e == f