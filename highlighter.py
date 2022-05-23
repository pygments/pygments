
import os
import sys
from pygments import highlight
from pygments.lexers import guess_lexer
from pygments.styles import get_all_styles
from pygments.formatters import HtmlFormatter

template = """
<!DOCTYPE html>
<html>
<head>
    <link href="{default_css_path}" rel="stylesheet" type="text/css" id="highlighter" />
    <style type="text/css">
        .highlight-select{{

        }}
    </style>
</head>
<body>

<div>
<div style="float:right">
<select class="highlight-select" onchange="change_highlight_style(this.value);">
    {html_select_highlight}
</select>
</div>
{html_content} 
<div>
</body>
<script>
function change_highlight_style(csspath){{
    document.getElementById("highlighter").setAttribute("href",csspath);
}}
</script>
</html
"""

extend_css_template = """
td.linenos .normal > a {{ color: {}; }}
td.linenos .special > a {{ color: {}; }}
td.linenos .normal {{ background-color: {}; }}
td.linenos .special {{ background-color: {}; }}
td.linenos {{ background-color: {}; }}
span.linenos {{ background-color: {}; }}

"""

EXTEND_CSS_CONF = {
    "material":{
        "linenos-color":"#FF5370", #line number font color
        "linenos-background-color":"#FFFFFF" #background color
    },
    "monokai":{
        "linenos-color":"#FF5370",
        "linenos-background-color":"#FFFFFF"
    },
}

class HighLighter:

    def __init__(self,code):
        self.code = code
        self.lexer = guess_lexer(code)

        self.css_dict = {} #Maintain a dictionary, the key is the name of the style, and the value is the CSS text of the style
        self.highlight_style = ""

        for style_name in get_all_styles():
            formatter = HtmlFormatter(style=style_name,linenos=True,anchorlinenos=True)
            css = formatter.get_style_defs('.highlight')
            self.add_style(style_name,css)

    def add_style(self,name,css):
        """add style"""

        self.css_dict[name] = css
        
        if not self.highlight_style:
            self.highlight_style = name

    def set_style(self,name):
        self.highlight_style = name

    def extend(self):

        for style in self.css_dict:
            if style in EXTEND_CSS_CONF:

                self.css_dict[style] += extend_css_template.format(
                    EXTEND_CSS_CONF[style]["linenos-color"],
                    EXTEND_CSS_CONF[style]["linenos-color"],
                    EXTEND_CSS_CONF[style]["linenos-background-color"],
                    EXTEND_CSS_CONF[style]["linenos-background-color"],
                    EXTEND_CSS_CONF[style]["linenos-background-color"],
                    EXTEND_CSS_CONF[style]["linenos-background-color"]
                    )

    def _save_css_files(self):

        #Determine whether the css folder exists, and create it if it does not exist
        if not os.path.exists("css"):
            os.mkdir("css")
        
        #save all css text as files
        for name in self.css_dict:
            with open("css/" + name + ".css",'w') as fp:
                fp.write(self.css_dict[name])

    def build(self):

        formatter =  HtmlFormatter(style=self.highlight_style,linenos=True,anchorlinenos=True)
        
        hl_html = highlight(self.code,self.lexer,formatter)

        hl_html_select = ""
        for style in self.css_dict:
            hl_html_select += '<option value="./css/{}.css">{}</option>'.format(style,style)

        
        return  template.format(
            html_select_highlight = hl_html_select,
            default_css_path = "./css/{}.css".format(self.highlight_style),
            html_content=hl_html
            )
        
    def save_as_html(self,file="index.html"):

        self._save_css_files()

        with open(file,'w') as fp:
            fp.write(self.build())


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print("Usage:\n\t",sys.argv[0],"codefile")
        exit()

    with open(sys.argv[1],'rb') as fp:
        code = fp.read().decode('utf-8')

    hler = HighLighter(code)
    hler.extend()
    hler.save_as_html()