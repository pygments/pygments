"""
    Contrast Tests
    ~~~~~~~~~~

    Pygments styles should be accessible to people with suboptimal vision.
    This test ensures that the minimum contrast of styles does not degrade,
    and that every rule of a new style fulfills the WCAG AA standard.[1]

    [1]: https://www.w3.org/WAI/WCAG21/Understanding/contrast-minimum.html
"""

import json
import os

import pygments.styles
import pygments.token
import wcag_contrast_ratio

JSON_FILENAME = os.path.join(os.path.dirname(__file__), "min_contrasts.json")
WCAG_AA_CONTRAST = 4.5


def hex2rgb(hexstr):
    hexstr = hexstr.lstrip("#")
    r = int(hexstr[:2], 16) / 255
    g = int(hexstr[2:4], 16) / 255
    b = int(hexstr[4:], 16) / 255
    return (r, g, b)


def get_style_contrasts(style_cls):
    return [
        (
            round(
                wcag_contrast_ratio.rgb(
                    hex2rgb(style["bgcolor"] or style_cls.background_color),
                    hex2rgb(style["color"] or "#000000")
                    # we default to black because browsers also do
                ),
                1,
            ),
            ttype,
        )
        for ttype, style in style_cls.list_styles()
        if ttype != pygments.token.Whitespace
    ]


def builtin_styles():
    for style_name in pygments.styles.STYLE_MAP:
        yield (style_name, pygments.styles.get_style_by_name(style_name))


def min_contrasts():
    return {
        name: min(x[0] for x in get_style_contrasts(style))
        for name, style in builtin_styles()
    }


def update_json():
    with open(JSON_FILENAME, "w", encoding="utf-8") as f:
        json.dump(
            min_contrasts(),
            f,
            indent=2,
        )


def test_contrasts(fail_if_improved=True):
    with open(JSON_FILENAME, encoding="utf-8") as f:
        previous_contrasts = json.load(f)

    for style_name in pygments.styles.STYLE_MAP:
        style = pygments.styles.get_style_by_name(style_name)
        contrasts = get_style_contrasts(style)
        min_contrast = min([x[0] for x in contrasts])

        bar = previous_contrasts.get(style_name, WCAG_AA_CONTRAST)

        assert not min_contrast < bar, (
            "contrast degradation for style '{}'\n"
            "The following rules have a contrast lower than the required {}:\n\n"
            "{}\n"
        ).format(
            style_name,
            bar,
            "\n".join(
                [
                    "* {:.2f} {}".format(contrast, ttype)
                    for contrast, ttype in contrasts
                    if contrast < bar
                ]
            ),
        )

        if fail_if_improved:
            assert (
                not min_contrast > bar
            ), "congrats, you improved a contrast! please run ./scripts/update_contrasts.py"
