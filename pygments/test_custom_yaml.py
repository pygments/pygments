# -*- coding: utf-8 -*-
import sys
import os

# Ensure the pygments module can be found
current_directory = os.path.dirname(os.path.abspath(__file__))
parent_directory = os.path.dirname(current_directory)
sys.path.insert(0, parent_directory)

from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers.custom_yaml import CustomYamlLexer

def test_yaml_highlighting():
    code = '''
{
  "foo": "foo",
   # foo:
  "bar": "bar",
   # baz: 0
  "baz": "baz",
   # baz
}
    '''

    lexer = CustomYamlLexer()
    formatter = HtmlFormatter(full=True, linenos=True)
    highlighted_code = highlight(code, lexer, formatter)

    # 将结果保存到 HTML 文件中
    output_file_path = "highlighted_code.html"
    with open(output_file_path, "w", encoding="utf-8") as f:
        f.write(highlighted_code)

# 执行测试函数
if __name__ == "__main__":
    test_yaml_highlighting()
