"""
    Pygments Pango Markup formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

import re

from pygments import highlight
from pygments.formatters import PangoMarkupFormatter
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

OUTPUT = r"""<tt><span fgcolor="#"><b>function</b></span> foobar(a, b) {
   <span fgcolor="#"><b>if</b></span> (a <span fgcolor="#">></span> b) {
      <span fgcolor="#"><b>return</b></span> a <span fgcolor="#">&amp;</span> b;
   }
   <span fgcolor="#"><b>if</b></span> (a <span fgcolor="#">&lt;</span> b) {
      <span fgcolor="#"><b>return</b></span> <span fgcolor="#"><b>true</b></span>;
   }
   console.log(<span fgcolor="#">"single quote ' and double quote \""</span>)
   console.log(<span fgcolor="#">'single quote \' and double quote "'</span>)
   <span fgcolor="#"><i>// comment with äöü ç
</i></span>}
</tt>"""

def test_correct_output():
    markup = highlight(INPUT, JavascriptLexer(), PangoMarkupFormatter())
    assert OUTPUT == re.sub('<span fgcolor="#[^"]{6}">', '<span fgcolor="#">', markup)
