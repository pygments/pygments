"""
    Pygments Pango Markup formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import pytest

from pygments import highlight
from pygments.formatters import PangoMarkupFormatter, NullFormatter
from pygments.lexers import JavascriptLexer

INPUT = r"""
function foobar(a, b) {
   if (a > b) {
      return a & b;
   }
   if (a < b) {
      return true;
   }
   console.log("single quote ' and double quote \"")
   console.log('single quote \' and double quote "')
   // comment with äöü ç
}
"""

OUTPUT = r"""<tt><span fgcolor="#008000"><b>function</b></span> foobar(a, b) {
   <span fgcolor="#008000"><b>if</b></span> (a <span fgcolor="#666666">></span> b) {
      <span fgcolor="#008000"><b>return</b></span> a <span fgcolor="#666666">&amp;</span> b;
   }
   <span fgcolor="#008000"><b>if</b></span> (a <span fgcolor="#666666">&lt;</span> b) {
      <span fgcolor="#008000"><b>return</b></span> <span fgcolor="#008000"><b>true</b></span>;
   }
   console.log(<span fgcolor="#BA2121">"single quote ' and double quote \""</span>)
   console.log(<span fgcolor="#BA2121">'single quote \' and double quote "'</span>)
   <span fgcolor="#408080"><i>// comment with äöü ç
</i></span>}
</tt>"""

def test_correct_output():
    assert OUTPUT == highlight(INPUT, JavascriptLexer(), PangoMarkupFormatter())
