load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin
    int_num = 1
    float_num = 0.1
    str = "A void map"
    array = (/1, 2, 3, 4, 5/)


    wks = gsn_open_wks("X11", "test_for_pygments")

    res = True
    res@mpMinLonF = 90.
    res@mpMaxLonF = 180.
    res@mpMinLatF = 0.
    res@mpMaxLatF = 90.

    plot = gsn_csm_map_ce(wks, res)
end