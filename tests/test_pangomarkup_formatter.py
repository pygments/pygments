"""
    Pygments Pango Markup formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-2025 by the Pygments team, see AUTHORS.
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

OUTPUT = r"""<tt><span fgcolor="#"><b>function</b></span><span fgcolor="#"> </span>foobar(a,<span fgcolor="#"> </span>b)<span fgcolor="#"> </span>{<span fgcolor="#">
   </span><span fgcolor="#"><b>if</b></span><span fgcolor="#"> </span>(a<span fgcolor="#"> </span><span fgcolor="#">></span><span fgcolor="#"> </span>b)<span fgcolor="#"> </span>{<span fgcolor="#">
      </span><span fgcolor="#"><b>return</b></span><span fgcolor="#"> </span>a<span fgcolor="#"> </span><span fgcolor="#">&amp;</span><span fgcolor="#"> </span>b;<span fgcolor="#">
   </span>}<span fgcolor="#">
   </span><span fgcolor="#"><b>if</b></span><span fgcolor="#"> </span>(a<span fgcolor="#"> </span><span fgcolor="#">&lt;</span><span fgcolor="#"> </span>b)<span fgcolor="#"> </span>{<span fgcolor="#">
      </span><span fgcolor="#"><b>return</b></span><span fgcolor="#"> </span><span fgcolor="#"><b>true</b></span>;<span fgcolor="#">
   </span>}<span fgcolor="#">
   </span>console.log(<span fgcolor="#">"single quote ' and double quote \""</span>)<span fgcolor="#">
   </span>console.log(<span fgcolor="#">'single quote \' and double quote "'</span>)<span fgcolor="#">
   </span><span fgcolor="#"><i>// comment with äöü ç</i></span><span fgcolor="#">
</span>}<span fgcolor="#">
</span></tt>"""

def test_correct_output():
    markup = highlight(INPUT, JavascriptLexer(), PangoMarkupFormatter())
    assert OUTPUT == re.sub('<span fgcolor="#[^"]{6}">', '<span fgcolor="#">', markup)
