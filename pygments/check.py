import sys
import array
import binascii
import itertools
import pygments.styles




"""


According the rule of WCAG to design this class
http://www.w3.org/TR/2008/REC-WCAG20-20081211/


The color distance metric calculates the Euclidean distance in the color space between each pair of clusters,
Ignore their size. The distance between colors A and B (using RGB) is calculated as the following function.

def color_distance(self,rgb1,rgb2)

For example:
style_control=check("monokai")
print(style_control.color_distance([0,0,255],[255,255,255]))



The following function is used to find the contrast rato
def check_contrast_rato(self,rgb1,rgb2)

contrast ratio
(L1 + 0.05) / (L2 + 0.05), where
L1 is the relative luminance of the lighter of the colors, and
L2 is the relative luminance of the darker of the colors.
https://www.w3.org/WAI/WCAG21/Understanding/contrast-minimum.html

For example:
style_control=check("monokai")
print(style_control.check_contrast_ratio([0,0,255],[255,255,255]))
output: 8.592471358428805
Which is mean the ratio
8.59:1


The following function is used to convert the hex to rgb
def hex_to_rgb(self,rgb1,rgb2)

For example:
style_control=check("monokai")
print(style_control.hex_to_rgb("#FF0000"))

output:
(255, 0, 0)


The following function is used to check contrast (Minimum)(Level AA)
passes_contrast_AA

Large Text
Large-scale text and images of large-scale text have a contrast ratio of at least 3:1;

Logotypes
Text that is part of a logo or brand name has no contrast requirement.


The following function is used to check contrast Level AAA
passes_contrast_AAA

The contrast ratio of 7:1 was chosen for level AAA because it compensated for the loss in contrast sensitivity
usually experienced by users with vision loss equivalent to approximately 20/80 vision.



The "get_color_similarity" function is used to find all the color similarities of a style.
It can find out the color difference of all colors of the style.

For example:
style_control=check("monokai")
print(style_control.get_color_similarity())



:copyright: Copyright 2006-2021 by the Pygments team, see AUTHORS.
:license: BSD, see LICENSE for details.

"""

class check:
    def __init__(self, style):
        self.style = style


    def color_distance(self,rgb1,rgb2):
        rm = (rgb1[0]+rgb2[0])* 0.5
        red_color_code = ((rm+2)*(rgb1[0]-rgb2[0]))**2
        green_color_code = ((rgb1[1]-rgb2[1])*4)**2
        blue_color_code = ((3-rm)*(rgb1[2]-rgb2[2]))**2
        return (red_color_code + green_color_code + blue_color_code) ** 0.5



    def check_contrast_ratio(self,c1,c2):
        l = c1 if sum(c2) < sum(c1) else c2
        d = c1 if sum(c2) > sum(c1) else c2

        contrast_ratio = ( self.get_relative_luminance(l) + 0.05 ) / ( self.get_relative_luminance(d) + 0.05 )
        return contrast_ratio

    def get_luminace(self,color_code):
        c = float(color_code)/255
        if c<0.03928:
            return c/12.92
        else:
            return ((0.055+c)/1.055) ** 2.4

    def get_relative_luminance(self,color):
        l1=self.get_luminace(color[0]) * 0.2126
        l2=self.get_luminace(color[1]) * 0.7152
        l3=self.get_luminace(color[2]) * 0.0722
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

    def get_style_color_rgb_list(self):
        colors=[]
        color_lists=[]
        for style in self.get_style_arr():
            if str(style).find(self.style) != -1:
                for arr in style:
                    color_lists.append(arr)

        for i in range(len(color_lists)):
            color_rgb="000000"
            obj=color_lists[i][0]
            n=str(obj).replace('Token.', '')
            if n!='':
                if color_lists[i][1]["color"]!=None:
                    color_rgb=self.hex_to_rgb(color_lists[i][1]["color"])
                    colors.append([n,color_rgb])
        return colors

    def get_style_color_list(self):
        color_names=[]
        color_rgbs=self.get_style_color_rgb_list()
        for c in color_rgbs:
            color_names.append(c[0])
        return color_names

    def get_style_color_rgb(self,color_name):
        color_rgb_list=self.get_style_color_rgb_list()
        for i in color_rgb_list:
            if i[0]==color_name:
                return i[1]


    def get_color_similarity(self):
        similarity_list=[]
        color_matchs=self.get_color_matchs()
        color_rgb_list=self.get_style_color_rgb_list()
        for c in color_matchs:
            c1=c[0]
            c2=c[1]
            c1_rgb=self.get_style_color_rgb(c1)
            c2_rgb=self.get_style_color_rgb(c2)
            similarity=self.color_distance(c1_rgb,c2_rgb)
            similarity_list.append([c1,c2,similarity])
        return tuple(similarity_list)

    def get_color_matchs(self):
        r=2
        color_list=self.get_style_color_list()
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

    def get_style_arr(self):
        style = []
        for style_name in pygments.styles.get_all_styles():
            style.append(pygments.styles.get_style_by_name(style_name))
        return style

    def get_style_bg_color(self):
        bgcolor=""
        for style in self.get_style_arr():
            if str(style).find(self.style) != -1:
                bgcolor = self.hex_to_rgb(style.background_color)
        return bgcolor

    def get_style_colors(self):
        colors=[]
        for style in self.get_style_arr():
            if str(style).find(self.style) != -1:
                for arr in style:
                    colors.append(arr)
        return colors
