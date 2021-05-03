import sys
import array
import binascii
import itertools


'''
Please kindly note that my currently design does not require any external libraries.
For instance, if pygment hasn't used NumPy library. I suggest don't use that library.
We need to write our own version library or function. 
So it is earlier to integrate into the existing class and convince the project owner to accept our changes.
'''


'''
http://www.w3.org/TR/2008/REC-WCAG20-20081211/
'''
class check:
    def __init__(self, style):
        self.style = style

    def color_distance(self,rgb1,rgb2):
        rm = (rgb1[0]+rgb2[0])* 0.5
        red_color_code = ((rm+2)*(rgb1[0]-rgb2[0]))**2
        green_color_code = ((rgb1[1]-rgb2[1])*4)**2
        blue_color_code = ((3-rm)*(rgb1[2]-rgb2[2]))**2
        return (red_color_code + green_color_code + blue_color_code) ** 0.5

    '''
    https://www.w3.org/WAI/WCAG21/Understanding/contrast-minimum.html
    '''
    def check_contrast_rato(self,c1,c2):
        l = c1 if sum(c2) < sum(c1) else c2
        d = c1 if sum(c2) > sum(c1) else c2

        contrast_ratio = ( get_relative_luminance(light) + 0.05 ) / ( get_relative_luminance(dark) + 0.05 )

    def get_hex_code(self,color):
        reg='^#?([a-f0-9]{3,3}|[a-f0-9]{6,6})$'
        result = re.match(reg, color)
        
        if result is None:
            raise Exception('Could not extract color')
            
        result = list(result.group(1))

        if len(result) == 6:
            result = [result[i] + result[i+1] for i in range(0, len(result), 2)]
        else:
            result = [result[i] + result[i] for i in range(0, len(result))]
            
        return [int(hex_code, 16) for hex_code in result]

    def get_luminace(self,color_code):
        c = float(color_code)/255
        if c<0.03928:
            return c/12.92
        else:
            return ((0.055+c)/1.055) ** 2.4

    def get_relative_luminance(self,color):
        l1=get_luminace(color[0]) * 0.2126 
        l2=get_luminace(color[1]) * 0.7152
        l3=get_luminace(color[2]) * 0.0722
        return l1+l2+l3

    def hex_to_rgb(self,hex_code):
        hex_code=hex_code.lstrip('#')
        return tuple(int(hex_code[i:i+2], 16) for i in (0, 2, 4))

    def parse_rgbcolor(self,bgcolor):
        if not bgcolor.startswith('#'):
            raise ValueError('RGB string must start with a "#"')
        return binascii.unhexlify(bgcolor[1:])

    def valid_rgb(self,rgbcolor):
        try:
            parse_rgbcolor(rgbcolor)
        except Exception as e:
            return False
        else:
            return True

    def valid_range(self,rgb):
        for r, g, b in (rgb1):
            if (not 0.0 <= r <= 1.0) or (not 0.0 <= g <= 1.0) or (not 0.0 <= b <= 1.0):
                raise ValueError("Invalid range (0.0 - 1.0)")

    def passes_contrast_AA(self,large,contrast):
        if large:
            return contrast >= 3.0
        else:
            return contrast >= 4.5

    def passes_contrast_AAA(self,large,contrast):
        if large:
            return contrast >= 4.5
        else:
            return contrast >= 7.0

    def color_combination(self,color_list, r):
        data = tuple(color_list)
        n = len(data)
        if r > n:
            return
        ind = list(range(r))
        yield tuple(data[i] for i in ind)
        while True:
            for i in reversed(range(r)):
                if ind[i] != (n - r) + i:
                    break
            else:
                return
            ind[i] = ind[i] + 1
            for j in range(i+1, r):
                ind[j] = 1+ind[j-1]
            yield tuple(data[i] for i in ind)
