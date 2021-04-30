import binascii
import array



def hex_to_rgb(msg):
    msg=msg.lstrip('#')
    return tuple(int(msg[i:i+2], 16) for i in (0, 2, 4))

def parse_rgbcolor(bgcolor):
    if not bgcolor.startswith('#'):
        raise ValueError('RGB string must start with a "#"')
    return binascii.unhexlify(bgcolor[1:])

def valid_rgb(rgbcolor):
    try:
        parse_rgbcolor(rgbcolor)
    except Exception as e:
        return False
    else:
        return True

def valid_range(rgb):
    for r, g, b in (rgb1):
        if (not 0.0 <= r <= 1.0) or (not 0.0 <= g <= 1.0) or (not 0.0 <= b <= 1.0):
            raise ValueError("Invalid range (0.0 - 1.0)")

def passes_contrast_AA(large=False,contrast):
    if large:
        return contrast >= 3.0
    else:
        return contrast >= 4.5


def passes_contrast_AAA(large=False,contrast):
    if large:
        return contrast >= 4.5
    else:
        return contrast >= 7.0
