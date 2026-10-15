"""
    Pygments image formatter tests
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    :copyright: Copyright 2006-present by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

from io import BytesIO

import pytest

from pygments import format
from pygments.formatters import img
from pygments.token import Token

pytest.importorskip('PIL')
from PIL import Image, ImageFont


@pytest.fixture
def image_font(monkeypatch):
    # Exercise real drawing without depending on fonts installed on the host.
    font = ImageFont.load_default()

    def init_fonts(self, font_name, font_size):
        self.fonts = dict.fromkeys(img.STYLES, font)
        self.variable = False

    monkeypatch.setattr(img.FontManager, '__init__', init_fonts)


@pytest.mark.parametrize('formatter_cls', [
    img.ImageFormatter, img.GifImageFormatter, img.JpgImageFormatter,
    img.BmpImageFormatter,
])
@pytest.mark.parametrize('line_numbers', [False, True])
def test_reuse_formatter_discards_previous_image(image_font, formatter_cls, line_numbers):
    formatter = formatter_cls(line_numbers=line_numbers)
    format([(Token.Keyword, 'WWWWWW\nold text\n')], formatter)
    tokens = [(Token.Text, 'i\n')]
    expected = format(tokens, formatter_cls(line_numbers=line_numbers))

    for _ in range(2):
        actual_image = Image.open(BytesIO(format(tokens, formatter))).convert('RGB')
        expected_image = Image.open(BytesIO(expected)).convert('RGB')
        assert actual_image.size == expected_image.size
        assert actual_image.tobytes() == expected_image.tobytes()
