# Example BitBake recipe exercising the BitBake lexer.
SUMMARY = "Hello world example"
DESCRIPTION = "A small recipe used to exercise the BitBake Pygments lexer."
HOMEPAGE = "https://example.com/${BPN}"
LICENSE = "MIT"
LIC_FILES_CHKSUM = "file://COPYING;md5=0000000000000000000000000000000000"

SRC_URI = " \
    https://example.com/${BPN}-${PV}.tar.gz \
    file://fix-build.patch \
"
SRC_URI[sha256sum] = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"

S = "${WORKDIR}/${BPN}-${PV}"

inherit autotools pkgconfig
inherit_defer ${PYTHON_PN}-setuptools
include_all conf/multilib.conf

DEPENDS = "zlib openssl"
RDEPENDS:${PN} = "bash"
RDEPENDS:${PN}:append:class-target = " systemd"
RRECOMMENDS:${PN}-doc = "man-pages"

EXTRA_OECONF = "--disable-static"
CFLAGS += "-O2"
CFLAGS:append:qemux86 = " -m32"

PACKAGECONFIG ??= "${@bb.utils.contains('DISTRO_FEATURES', 'systemd', 'systemd', '', d)}"
PACKAGECONFIG[systemd] = "--with-systemd,--without-systemd,systemd"

do_compile:prepend() {
    echo "preparing build"
}

do_install:append() {
    install -d ${D}${bindir}
    install -m 0755 ${B}/hello ${D}${bindir}/hello
}

fakeroot do_rootfs() {
    echo "stage rootfs into ${IMAGE_ROOTFS}"
}

python do_check() {
    bb.warn("checking %s" % d.getVar('PN'))
}

fakeroot python () {
    d.setVar('FOO', 'bar')
}

def my_helper(d):
    return d.getVar('PN') + '-helper'

addtask check after do_compile before do_install
deltask do_package_qa
EXPORT_FUNCTIONS do_install do_compile

do_install[depends] = "qemu-native:do_populate_sysroot"
do_compile[noexec] = "1"
