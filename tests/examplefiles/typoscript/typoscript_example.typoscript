# ***************************************************************************
# Notice: "styles." (and "temp.") objects are UNSET after template parsing!
# Use "lib." for persisting storage of objects.
# ***************************************************************************

<INCLUDE_TYPOSCRIPT: source="FILE: EXT:www_tue_nl/Configuration/TypoScript/Setup/Root.ts">

page.80 = RECORDS
page.80 {
      source = 1
      tables = tt_address
      conf.tt_address = COA
      conf.tt_address {
              20 = TEXT
              20.field = email
              20.typolink.parameter.field = email
      }
}

	/*
page.200 = PHP_SCRIPT_EXT
page.200 {
  1 = TMENU
  1.wrap = <div style="width:200px; border: 1px solid;">|</div>
  1.expAll = 1
  1.submenuObjSuffixes = a |*|  |*| b
  1.NO.allWrap = <b>|</b><br/>

  2 = TMENU
  2.NO.allWrap = <div style="background:red;">|</div>

  2a = TMENU
  2a.NO.allWrap = <div style="background:yellow;">|</div>
*
  2b = TMENU
  2b.NO.allWrap = <div style="background:green;">|</div>
}
*/

   # Add the CSS and JS files
page {
   includeCSS { # comment at the end of a line
      file99 = fileadmin/your-fancybox.css
   }
   includeJSFooter {
      fancybox = fileadmin/your-fancybox.js
   }
}

   # Change the default rendering of images to match lightbox requirements
tt_content.image.20.1.imageLinkWrap {
   JSwindow = 0
   test = MyExtension\Path\To\Class

   directImageLink = 1
   linkParams.ATagParams {
      dataWrap = class= "lightbox" rel="fancybox{field:uid}"
   }
}

tt_content.image.20.1.imageLinkWrap >
tt_content.image.20.1.imageLinkWrap = 1
tt_content.image.20.1.imageLinkWrap {
   enable = 1
   typolink {
         # directly link to the recent image
      parameter.cObject = IMG_RESOURCE
      parameter.cObject.file.import.data = TSFE:lastImageInfo|origFile
      parameter.cObject.file.maxW = {$styles.content.imgtext.maxW}
      parameter.override.listNum.stdWrap.data = register : IMAGE_NUM_CURRENT
      title.field = imagecaption // title
      title.split.token.char = 10
      title.if.isTrue.field = imagecaption // header
      title.split.token.char = 10
      title.split.returnKey.data = register : IMAGE_NUM_CURRENT
      parameter.cObject = IMG_RESOURCE
      parameter.cObject.file.import.data = TSFE:lastImageInfo|origFile
      ATagParams = target="_blank"
   }
}

10 = IMAGE
10 {
      # point to the image
   file = fileadmin/demo/lorem_ipsum/images/a4.jpg
      # make it rather small
   file.width = 80
      # add a link to tx_cms_showpic.php that shows the original image
   imageLinkWrap = 1
   imageLinkWrap {
	       enable = 1
	       # JSwindow = 1
	    }
}

# Clear out any constants in this reserved room!
styles.content >

# get content
styles.content.get = CONTENT
styles.content.get {
	table = tt_content
	select.orderBy = sorting
	select.where = colPos=0
	select.languageField = sys_language_uid
}

# get content, left
styles.content.getLeft < styles.content.get
styles.content.getLeft.select.where = colPos=1

# get content, right
styles.content.getRight < styles.content.get
styles.content.getRight.select.where = colPos=2

# get content, margin
styles.content.getBorder < styles.content.get
styles.content.getBorder.select.where = colPos=3

# get news
styles.content.getNews < styles.content.get
styles.content.getNews.select.pidInList = {$styles.content.getNews.newsPid}

# Edit page object:
styles.content.editPanelPage = COA
styles.content.editPanelPage {
	10 = EDITPANEL
	10 {
		allow = toolbar,move,hide
		label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.page
		label.wrap = |&nbsp;<b>%s</b>
	}
}











# *********************************************************************
# "lib." objects are preserved from unsetting after template parsing
# *********************************************************************

# Creates persistent ParseFunc setup for non-HTML content. This is recommended to use (as a reference!)
lib.parseFunc {
	makelinks = 1
	makelinks.http.keep = {$styles.content.links.keep}
	makelinks.http.extTarget = {$styles.content.links.extTarget}
	makelinks.mailto.keep = path
	tags {
		link = TEXT
		link {
			current = 1
			typolink.parameter.data = parameters : allParams
			typolink.extTarget = {$styles.content.links.extTarget}
			typolink.target = {$styles.content.links.target}
			parseFunc.constants =1
		}
	}
	allowTags = {$styles.content.links.allowTags}
	denyTags = *
	sword = <span class="csc-sword">|</span>
	constants = 1

	nonTypoTagStdWrap.HTMLparser = 1
	nonTypoTagStdWrap.HTMLparser {
		keepNonMatchedTags = 1
		htmlSpecialChars = 2
	}
}

# good old parsefunc in "styles.content.parseFunc" is created for backwards compatibility. Don't use it, just ignore.
styles.content.parseFunc < lib.parseFunc

# Creates persistent ParseFunc setup for RTE content (which is mainly HTML) based on the "ts_css" transformation.
lib.parseFunc_RTE < lib.parseFunc
lib.parseFunc_RTE {
	//  makelinks >
	# Processing <table> and <blockquote> blocks separately
	externalBlocks = table, blockquote, dd, dl, ol, ul, div
	externalBlocks {
		# The blockquote content is passed into parseFunc again...
		blockquote.stripNL=1
		blockquote.callRecursive=1
		blockquote.callRecursive.tagStdWrap.HTMLparser = 1
		blockquote.callRecursive.tagStdWrap.HTMLparser.tags.blockquote.overrideAttribs = style="margin-bottom:0;margin-top:0;"

		ol.stripNL=1
		ol.stdWrap.parseFunc = < lib.parseFunc

		ul.stripNL=1
		ul.stdWrap.parseFunc = < lib.parseFunc

		table.stripNL=1
		table.stdWrap.HTMLparser = 1
		table.stdWrap.HTMLparser.tags.table.fixAttrib.class {
			default = contenttable
			always = 1
			list = contenttable
		}
		table.stdWrap.HTMLparser.keepNonMatchedTags = 1
		table.HTMLtableCells=1
		table.HTMLtableCells {
			default.callRecursive=1
			addChr10BetweenParagraphs=1
		}
		div.stripNL = 1
		div.callRecursive = 1

		# Definition list processing
		dl < .div
		dd < .div
	}
	nonTypoTagStdWrap.encapsLines {
		encapsTagList = p,pre,h1,h2,h3,h4,h5,h6,hr,dt
		remapTag.DIV = P
		nonWrappedTag = P
		innerStdWrap_all.ifBlank = &nbsp;
		addAttributes.P.class = bodytext
		addAttributes.P.class.setOnly=blank
	}
	nonTypoTagStdWrap.HTMLparser = 1
	nonTypoTagStdWrap.HTMLparser {
		keepNonMatchedTags = 1
		htmlSpecialChars = 2
	}
}


# Content header:
lib.stdheader = COA
lib.stdheader {

	# Create align style-attribute for <Hx> tags
	2 = LOAD_REGISTER
	2.headerStyle.field = header_position
	2.headerStyle.required = 1
	2.headerStyle.noTrimWrap = | style="text-align:|;"|

	# Create class="csc-firstHeader" attribute for <Hx> tags
	3 = LOAD_REGISTER
	3.headerClass = csc-firstHeader
	3.headerClass.if.value=1
	3.headerClass.if.equals.data = cObj:parentRecordNumber
	3.headerClass.noTrimWrap = | class="|"|

	# Date format:
	5 = TEXT
	5.field = date
	5.if.isTrue.field = date
	5.strftime = %x
	5.wrap = <p class="csc-header-date">|</p>
	5.prefixComment = 2 | Header date:

	# This CASE cObject renders the header content:
	# currentValue is set to the header data, possibly wrapped in link-tags.
	10 = CASE
	10.setCurrent {
		field = header
		htmlSpecialChars = 1
		typolink.parameter.field = header_link
	}
	10.key.field = header_layout
	10.key.ifEmpty = {$content.defaultHeaderType}
	10.key.ifEmpty.override.data = register: defaultHeaderType

	10.1 = TEXT
	10.1.current = 1
	10.1.dataWrap = <h1{register:headerStyle}{register:headerClass}>|</h1>

	10.2 < .10.1
	10.2.dataWrap = <h2{register:headerStyle}{register:headerClass}>|</h2>

	10.3 < .10.1
	10.3.dataWrap = <h3{register:headerStyle}{register:headerClass}>|</h3>

	10.4 < .10.1
	10.4.dataWrap = <h4{register:headerStyle}{register:headerClass}>|</h4>

	10.5 < .10.1
	10.5.dataWrap = <h5{register:headerStyle}{register:headerClass}>|</h5>

	# Pops the used registers off the stack:
	98 = RESTORE_REGISTER
	99 = RESTORE_REGISTER

	# Post-processing:
	stdWrap.fieldRequired = header
	stdWrap.if {
		equals.field = header_layout
		value = 100
		negate = 1
	}

	stdWrap.editIcons = tt_content : header, [header_layout | header_position], [header_link|date]
	stdWrap.editIcons.beforeLastTag = 1
	stdWrap.editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.header

	stdWrap.dataWrap = <div class="csc-header csc-header-n{cObj:parentRecordNumber}">|</div>
	stdWrap.prefixComment = 2 | Header:
}















#******************************************************
# Including library for processing of some elements:
#******************************************************
includeLibs.tx_cssstyledcontent_pi1 = EXT:css_styled_content/pi1/class.tx_cssstyledcontent_pi1.php


#**********************************
# tt_content is started
#**********************************
tt_content >
tt_content = CASE
tt_content.key.field = CType
tt_content.stdWrap {
	innerWrap.cObject = CASE
	innerWrap.cObject {
		key.field = section_frame

		default = COA
		default {
			10 = TEXT
			10 {
				value = <div id="c{field:uid}"
				override.cObject = TEXT
				override.cObject {
					value = <div
					if.value = div
					if.equals.field = CType
				}
				insertData = 1
			}

			15 = TEXT
			15 {
				value = csc-default
				noTrimWrap = | class="|" |
				required = 1
			}

			20 = COA
			20 {
				10 = COA
				10 {
					10 = TEXT
					10 {
						value = {$content.spaceBefore}
						wrap = |+
						if.isTrue = {$content.spaceBefore}
					}

					20 = TEXT
					20 {
						field = spaceBefore
					}

			 		stdWrap {
						prioriCalc = intval
						wrap = margin-top:|px;
						required = 1
						ifEmpty.value =
					}
				}

				20 = COA
				20 {
					10 = TEXT
					10 {
						value = {$content.spaceAfter}
						wrap = |+
						if.isTrue = {$content.spaceAfter}
					}

					20 = TEXT
					20 {
						field = spaceAfter
					}

					stdWrap {
						prioriCalc = intval
						wrap = margin-bottom:|px;
						required = 1
						ifEmpty.value =
					}
				}

				stdWrap.noTrimWrap = | style="|" |
				stdWrap.required = 1
			}
			30 = TEXT
			30.value = >|</div>
		}

		1 =< tt_content.stdWrap.innerWrap.cObject.default
		1.15.value = csc-frame csc-frame-invisible

		5 =< tt_content.stdWrap.innerWrap.cObject.default
		5.15.value = csc-frame csc-frame-rulerBefore

		6 =< tt_content.stdWrap.innerWrap.cObject.default
		6.15.value = csc-frame csc-frame-rulerAfter

		10 =< tt_content.stdWrap.innerWrap.cObject.default
		10.15.value = csc-frame csc-frame-indent

		11 =< tt_content.stdWrap.innerWrap.cObject.default
		11.15.value = csc-frame csc-frame-indent3366

		12 =< tt_content.stdWrap.innerWrap.cObject.default
		12.15.value = csc-frame csc-frame-indent6633

		20 =< tt_content.stdWrap.innerWrap.cObject.default
		20.15.value = csc-frame csc-frame-frame1

		21 =< tt_content.stdWrap.innerWrap.cObject.default
		21.15.value = csc-frame csc-frame-frame2

		66 = COA
		66 {
			10 = TEXT
			10 {
				value = <a id="c{field:uid}"></a>
				insertData = 1
			}

			20 = COA
			20 {
				10 = TEXT
				10 {
					value = {$content.spaceBefore}
					wrap = |+
					if.isTrue = {$content.spaceBefore}
				}

				20 = TEXT
				20 {
					field = spaceBefore
				}

		 		stdWrap {
					prioriCalc = intval
					wrap = margin-top:|px;
					required = 1
					ifEmpty.value =
					wrap2 = <div style="|"></div>
				}
			}

			30 = TEXT
			30 {
				value = |
			}

			40 < .20
			40 {
				10 {
					value = {$content.spaceAfter}
					if.isTrue = {$content.spaceAfter}
				}
				20.field = spaceAfter
				stdWrap.wrap = margin-bottom:|px;
			}
		}

	}

	innerWrap2 = | <p class="csc-linkToTop"><a href="#">{LLL:EXT:css_styled_content/pi1/locallang.xml:label.toTop}</a></p>
	innerWrap2.insertData = 1
	innerWrap2.fieldRequired = linkToTop

	prepend = TEXT
	prepend.dataWrap = <a id="c{field:_LOCALIZED_UID}"></a>
	prepend.if.isTrue.field = _LOCALIZED_UID

	editPanel = 1
	editPanel {
		allow = move,new,edit,hide,delete
		line = 5
		label = %s
		onlyCurrentPid = 1
		previewBorder = 4
		edit.displayRecord = 1
	}

	prefixComment = 1 | CONTENT ELEMENT, uid:{field:uid}/{field:CType}
}



# *****************
# CType: header
# *****************
# See Object path "lib.stdheader"
tt_content.header = COA
tt_content.header {
	10 = < lib.stdheader

	20 = TEXT
	20 {
		field = subheader
		required = 1

		dataWrap = <p class="csc-subheader csc-subheader-{field:layout}">|</p>
		htmlSpecialChars = 1

		editIcons = tt_content:subheader,layout
		editIcons.beforeLastTag = 1
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.subheader

		prefixComment = 2 | Subheader:
	}
}



# *****************
# CType: text
# *****************
tt_content.text = COA
tt_content.text {
	10 = < lib.stdheader

	20 = TEXT
	20 {
		field = bodytext
		required = 1

		parseFunc = < lib.parseFunc_RTE

		editIcons = tt_content:bodytext, rte_enabled
		editIcons.beforeLastTag = 1
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.bodytext

		prefixComment = 2 | Text:
	}
}



# *****************
# CType: image
# *****************
# (also used for rendering 'textpic' type):
tt_content.image = COA
tt_content.image.10 = < lib.stdheader
tt_content.image.20 = USER
tt_content.image.20 {
	userFunc = tx_cssstyledcontent_pi1->render_textpic

	# Image source
	imgList.field = image
	imgPath = uploads/pics/

	# Single image rendering
	imgObjNum = 1
	1 {
		file.import.current = 1
		file.width.field = imagewidth
		imageLinkWrap = 1
		imageLinkWrap {
			bodyTag = <body style="margin:0; background:#fff;">
			wrap = <a href="javascript:close();"> | </a>
			width = {$styles.content.imgtext.linkWrap.width}
			height = {$styles.content.imgtext.linkWrap.height}
			effects = {$styles.content.imgtext.linkWrap.effects}

			JSwindow = 1
			JSwindow.newWindow = {$styles.content.imgtext.linkWrap.newWindow}
			JSwindow.if.isFalse = {$styles.content.imgtext.linkWrap.lightboxEnabled}

			directImageLink = {$styles.content.imgtext.linkWrap.lightboxEnabled}

			enable.field = image_zoom
			enable.ifEmpty.typolink.parameter.field = image_link
			enable.ifEmpty.typolink.parameter.listNum.splitChar = 10
			enable.ifEmpty.typolink.parameter.listNum.stdWrap.data = register : IMAGE_NUM_CURRENT
			enable.ifEmpty.typolink.returnLast = url

			typolink.parameter.field = image_link
			typolink.parameter.listNum.splitChar = 10
			typolink.parameter.listNum.stdWrap.data = register : IMAGE_NUM_CURRENT
			typolink.target = {$styles.content.links.target}
			typolink.extTarget = {$styles.content.links.extTarget}

			linkParams.ATagParams.dataWrap =  class="{$styles.content.imgtext.linkWrap.lightboxCssClass}" rel="{$styles.content.imgtext.linkWrap.lightboxRelAttribute}"
		}

		altText = TEXT
		altText {
			field = altText
			stripHtml = 1
			split.token.char = 10
			split.token.if.isTrue = {$styles.content.imgtext.imageTextSplit}
			split.returnKey.data = register : IMAGE_NUM_CURRENT
		}

		titleText < .altText
		titleText.field = titleText

		longdescURL < .altText
		longdescURL.field = longdescURL

		emptyTitleHandling = {$styles.content.imgtext.emptyTitleHandling}
		titleInLink = {$styles.content.imgtext.titleInLink}
		titleInLinkAndImg = {$styles.content.imgtext.titleInLinkAndImg}
	}

	textPos.field = imageorient
	maxW = {$styles.content.imgtext.maxW}
	maxW.override.data = register:maxImageWidth
	maxWInText = {$styles.content.imgtext.maxWInText}
	maxWInText.override.data = register:maxImageWidthInText

	equalH.field = imageheight

	image_compression.field = image_compression
	image_effects.field = image_effects

	noRows.field = image_noRows

	cols.field = imagecols
	border.field = imageborder

	caption {
		1 = TEXT
		1 {
			field = imagecaption
			required = 1
			parseFunc =< lib.parseFunc
			br = 1
			split.token.char = 10
			split.token.if.isPositive = {$styles.content.imgtext.imageTextSplit} + {$styles.content.imgtext.captionSplit}
			split.returnKey.data = register : IMAGE_NUM_CURRENT
		}
	}
	# captionSplit is deprecated, use imageTextSplit instead
	captionSplit = {$styles.content.imgtext.captionSplit}
	captionAlign.field = imagecaption_position
	# caption/alttext/title/longdescURL splitting
	imageTextSplit = {$styles.content.imgtext.imageTextSplit}

	borderCol = {$styles.content.imgtext.borderColor}
	borderThick = {$styles.content.imgtext.borderThick}
	borderClass = {$styles.content.imgtext.borderClass}
	colSpace = {$styles.content.imgtext.colSpace}
	rowSpace = {$styles.content.imgtext.rowSpace}
	textMargin = {$styles.content.imgtext.textMargin}

	borderSpace = {$styles.content.imgtext.borderSpace}
	separateRows = {$styles.content.imgtext.separateRows}
	addClasses =
	addClassesImage =
	addClassesImage.ifEmpty = csc-textpic-firstcol csc-textpic-lastcol
	addClassesImage.override = csc-textpic-firstcol |*| |*| csc-textpic-lastcol
	addClassesImage.override.if {
		isGreaterThan.field = imagecols
		value = 1
	}

	#
	imageStdWrap.dataWrap = <div class="csc-textpic-imagewrap" style="width:{register:totalwidth}px;"> | </div>
	imageStdWrapNoWidth.wrap = <div class="csc-textpic-imagewrap"> | </div>

	# if noRows is set, wrap around each column:
	imageColumnStdWrap.dataWrap = <div class="csc-textpic-imagecolumn" style="width:{register:columnwidth}px;"> | </div>

	layout = CASE
	layout {
		key.field = imageorient
		# above-center
		default = TEXT
		default.value = <div class="csc-textpic csc-textpic-center csc-textpic-above###CLASSES###">###IMAGES######TEXT###</div><div class="csc-textpic-clear"><!-- --></div>
		# above-right
		1 = TEXT
		1.value = <div class="csc-textpic csc-textpic-right csc-textpic-above###CLASSES###">###IMAGES######TEXT###</div><div class="csc-textpic-clear"><!-- --></div>
		# above-left
		2 = TEXT
		2.value = <div class="csc-textpic csc-textpic-left csc-textpic-above###CLASSES###">###IMAGES######TEXT###</div><div class="csc-textpic-clear"><!-- --></div>
		# below-center
		8 = TEXT
		8.value = <div class="csc-textpic csc-textpic-center csc-textpic-below###CLASSES###">###TEXT######IMAGES###</div><div class="csc-textpic-clear"><!-- --></div>
		# below-right
		9 = TEXT
		9.value = <div class="csc-textpic csc-textpic-right csc-textpic-below###CLASSES###">###TEXT######IMAGES###</div><div class="csc-textpic-clear"><!-- --></div>
		# below-left
		10 = TEXT
		10.value = <div class="csc-textpic csc-textpic-left csc-textpic-below###CLASSES###">###TEXT######IMAGES###</div><div class="csc-textpic-clear"><!-- --></div>
		# intext-right
		17 = TEXT
		17.value = <div class="csc-textpic csc-textpic-intext-right###CLASSES###">###IMAGES######TEXT###</div>
		17.override = <div class="csc-textpic csc-textpic-intext-right###CLASSES###">###IMAGES######TEXT###</div><div class="csc-textpic-clear"><!-- --></div>
		17.override.if.isTrue = {$styles.content.imgtext.addIntextClearer}
		# intext-left
		18 = TEXT
		18.value = <div class="csc-textpic csc-textpic-intext-left###CLASSES###">###IMAGES######TEXT###</div>
		18.override = <div class="csc-textpic csc-textpic-intext-left###CLASSES###">###IMAGES######TEXT###</div><div class="csc-textpic-clear"><!-- --></div>
		18.override.if.isTrue = {$styles.content.imgtext.addIntextClearer}
		# intext-right-nowrap
		25 = TEXT
		25.value = <div class="csc-textpic csc-textpic-intext-right-nowrap###CLASSES###">###IMAGES###<div style="margin-right:{register:rowWidthPlusTextMargin}px;">###TEXT###</div></div><div class="csc-textpic-clear"><!-- --></div>
		25.insertData = 1
		# intext-left-nowrap
		26 = TEXT
		26.value = <div class="csc-textpic csc-textpic-intext-left-nowrap###CLASSES###">###IMAGES###<div style="margin-left:{register:rowWidthPlusTextMargin}px;">###TEXT###</div></div><div class="csc-textpic-clear"><!-- --></div>
		26.insertData = 1
	}

	rendering {
		dl {
			# Choose another rendering for special edge cases
			fallbackRendering = COA
			fallbackRendering {
				# Just one image without a caption => don't need the dl-overhead, use the "simple" rendering
				10 = TEXT
				10 {
					if {
						isFalse.field = imagecaption
						value = 1
						equals.data = register:imageCount
					}
					value = simple
				}

				# Multiple images and one global caption => "ul"
				20 = TEXT
				20 {
					if {
						value = 1
						isGreaterThan.data = register:imageCount
						isTrue.if.isTrue.data = register:renderGlobalCaption
						isTrue.field = imagecaption
					}
					value = ul
				}

				# Multiple images and no caption at all => "ul"
				30 = TEXT
				30 {
					if {
						value = 1
						isGreaterThan.data = register:imageCount
						isFalse.field = imagecaption
					}
					value = ul
				}
			}
			imageRowStdWrap.dataWrap = <div class="csc-textpic-imagerow" style="width:{register:rowwidth}px;"> | </div>
			imageLastRowStdWrap.dataWrap = <div class="csc-textpic-imagerow csc-textpic-imagerow-last" style="width:{register:rowwidth}px;"> | </div>
			noRowsStdWrap.wrap =
			oneImageStdWrap.dataWrap = <dl class="csc-textpic-image###CLASSES###" style="width:{register:imagespace}px;"> | </dl>
			imgTagStdWrap.wrap = <dt> | </dt>
			editIconsStdWrap.wrap = <dd> | </dd>
			caption {
				required = 1
				wrap = <dd class="csc-textpic-caption"> | </dd>
			}
		}
		ul {
			# Just one image without a caption => don't need the ul-overhead, use the "simple" rendering
			fallbackRendering < tt_content.image.20.rendering.dl.fallbackRendering.10
			imageRowStdWrap.dataWrap = <div class="csc-textpic-imagerow" style="width:{register:rowwidth}px;"><ul> | </ul></div>
			imageLastRowStdWrap.dataWrap = <div class="csc-textpic-imagerow csc-textpic-imagerow-last" style="width:{register:rowwidth}px;"><ul> | </ul></div>
			noRowsStdWrap.wrap = <ul> | </ul>
			oneImageStdWrap.dataWrap = <li class="csc-textpic-image###CLASSES###" style="width:{register:imagespace}px;"> | </li>
			imgTagStdWrap.wrap =
			editIconsStdWrap.wrap = <div> | </div>
			caption.wrap = <div class="csc-textpic-caption"> | </div>
		}
		div {
			# Just one image without a caption => don't need the div-overhead, use the "simple" rendering
			fallbackRendering < tt_content.image.20.rendering.dl.fallbackRendering.10
			imageRowStdWrap.dataWrap = <div class="csc-textpic-imagerow" style="width:{register:rowwidth}px;"> | </div>
			imageLastRowStdWrap.dataWrap = <div class="csc-textpic-imagerow csc-textpic-imagerow-last" style="width:{register:rowwidth}px;"> | </div>
			noRowsStdWrap.wrap =
			oneImageStdWrap.dataWrap = <div class="csc-textpic-image###CLASSES###" style="width:{register:imagespace}px;"> | </div>
			imgTagStdWrap.wrap = <div> | </div>
			editIconsStdWrap.wrap = <div> | </div>
			caption.wrap = <div class="csc-textpic-caption"> | </div>
		}
		simple {
			imageRowStdWrap.dataWrap = |
			imageLastRowStdWrap.dataWrap = |
			noRowsStdWrap.wrap =
			oneImageStdWrap.dataWrap = |
			imgTagStdWrap.wrap = |
			editIconsStdWrap.wrap = |
			caption.wrap = <div class="csc-textpic-caption"> | </div>
			imageStdWrap.dataWrap = <div class="csc-textpic-imagewrap csc-textpic-single-image" style="width:{register:totalwidth}px;"> | </div>
			imageStdWrapNoWidth.wrap = <div class="csc-textpic-imagewrap csc-textpic-single-image"> | </div>
		}
	}
	renderMethod = dl

	editIcons = tt_content : image [imageorient|imagewidth|imageheight], [imagecols|image_noRows|imageborder],[image_link|image_zoom],[image_compression|image_effects|image_frames],imagecaption[imagecaption_position]
	editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.images

	caption.editIcons = tt_content : imagecaption[imagecaption_position]
	caption.editIcons.beforeLastTag=1
	caption.editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.caption

	stdWrap.prefixComment = 2 | Image block:
}

# *****************
# CType: textpic
# *****************
tt_content.textpic = COA
tt_content.textpic {
	10 = COA
	10.if.value = 25
	10.if.isLessThan.field = imageorient
	10.10 = < lib.stdheader

	20  = < tt_content.image.20
	20 {
		text.10 = COA
		text.10 {
			if.value = 24
			if.isGreaterThan.field = imageorient
			10 = < lib.stdheader
			10.stdWrap.dataWrap = <div class="csc-textpicHeader csc-textpicHeader-{field:imageorient}">|</div>
		}
		text.20 = < tt_content.text.20
		text.wrap = <div class="csc-textpic-text"> | </div>
	}
}



# *****************
# CType: bullet
# *****************
tt_content.bullets = COA
tt_content.bullets {
	10 = < lib.stdheader

	20 = TEXT
	20 {
		field = bodytext
		trim = 1
		split{
			token.char = 10
			cObjNum = |*|1|| 2|*|
			1.current = 1
			1.parseFunc =< lib.parseFunc
			1.wrap = <li class="odd">|</li>

			2.current = 1
			2.parseFunc =< lib.parseFunc
			2.wrap = <li class="even">|</li>
		}
		dataWrap = <ul class="csc-bulletlist csc-bulletlist-{field:layout}">|</ul>
	 	editIcons = tt_content: bodytext, [layout]
	 	editIcons.beforeLastTag = 1
	 	editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.php:eIcon.bullets

	 	prefixComment = 2 | Bullet list:
	}
}



# *****************
# CType: table
# *****************
# Rendered by a PHP function specifically written to handle CE tables. See css_styled_content/pi1/class.tx_cssstyledcontent_pi1.php
tt_content.table = COA
tt_content.table {
	10 = < lib.stdheader

	20 = USER
	20.userFunc = tx_cssstyledcontent_pi1->render_table
	20.field = bodytext

	20.color {
		default =
		1 = #EDEBF1
		2 = #F5FFAA
	}
	20.tableParams_0 {
		border =
		cellpadding =
		cellspacing =
	}
	20.tableParams_1 {
		border =
		cellpadding =
		cellspacing =
	}
	20.tableParams_2 {
		border =
		cellpadding =
		cellspacing =
	}
	20.tableParams_3 {
		border =
		cellpadding =
		cellspacing =
	}
	20.innerStdWrap.wrap = |
	20.innerStdWrap.parseFunc = < lib.parseFunc

	20.stdWrap {
		editIcons = tt_content: cols, bodytext, [layout], [table_bgColor|table_border|table_cellspacing|table_cellpadding]
		editIcons.beforeLastTag = 1
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.table

		prefixComment = 2 | Table:
	}
}


# *****************
# CType: uploads
# *****************
# Rendered by a PHP function specifically written to handle CE filelists. See css_styled_content/pi1/class.tx_cssstyledcontent_pi1.php
tt_content.uploads = COA
tt_content.uploads {
	10 = < lib.stdheader

	20 = USER
	20.userFunc = tx_cssstyledcontent_pi1->render_uploads
	20.field = media
	20.filePath.field = select_key

	20 {
		# Rendering for each file (e.g. rows of the table) as a cObject
		itemRendering = COA
		itemRendering {
			wrap = <tr class="tr-odd tr-first">|</tr> |*| <tr class="tr-even">|</tr> || <tr class="tr-odd">|</tr> |*|

			10 = TEXT
			10.data = register:linkedIcon
			10.wrap = <td class="csc-uploads-icon">|</td>
			10.if.isPositive.field = layout

			20 = COA
			20.wrap = <td class="csc-uploads-fileName">|</td>
			20.1 = TEXT
			20.1 {
				data = register:linkedLabel
				wrap = <p>|</p>
			}
			20.2 = TEXT
			20.2 {
				data = register:description
				wrap = <p class="csc-uploads-description">|</p>
				required = 1
				htmlSpecialChars = 1
			}

			30 = TEXT
			30.if.isTrue.field = filelink_size
			30.data = register:fileSize
			30.wrap = <td class="csc-uploads-fileSize">|</td>
			30.bytes = 1
			30.bytes.labels = {$styles.content.uploads.filesizeBytesLabels}
		}
		useSpacesInLinkText = 0
		stripFileExtensionFromLinkText = 0
	}

	20.color {
		default =
		1 = #EDEBF1
		2 = #F5FFAA
	}
	20.tableParams_0 {
		border =
		cellpadding =
		cellspacing =
	}
	20.tableParams_1 {
		border =
		cellpadding =
		cellspacing =
	}
	20.tableParams_2 {
		border =
		cellpadding =
		cellspacing =
	}
	20.tableParams_3 {
		border =
		cellpadding =
		cellspacing =
	}

	20.linkProc {
		target = _blank
		jumpurl = {$styles.content.uploads.jumpurl}
		jumpurl.secure = {$styles.content.uploads.jumpurl_secure}
		jumpurl.secure.mimeTypes = {$styles.content.uploads.jumpurl_secure_mimeTypes}
		removePrependedNumbers = 1

		iconCObject = IMAGE
		iconCObject.file.import.data = register : ICON_REL_PATH
		iconCObject.file.width = 150
	}

	20.filesize {
		bytes = 1
		bytes.labels = {$styles.content.uploads.filesizeBytesLabels}
	}

	20.stdWrap {
		editIcons = tt_content: media, layout [table_bgColor|table_border|table_cellspacing|table_cellpadding], filelink_size, imagecaption
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.filelist

		prefixComment = 2 | File list:
	}
}


# ******************
# CType: multimedia
# ******************
tt_content.multimedia = COA
tt_content.multimedia {
	10 = < lib.stdheader

	20 = MULTIMEDIA
	20.file.field = multimedia
	20.file.wrap = uploads/media/
	20.file.listNum = 0
	20.params.field = bodytext

	20.stdWrap {
		editIcons = tt_content: multimedia, bodytext
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.multimedia

		prefixComment = 2 | Multimedia element:
	}
}

# *****************
# CType: swfobject
# *****************
tt_content.swfobject = COA
tt_content.swfobject {
	10 = < lib.stdheader

	20 = SWFOBJECT
	20 {
		file =
		width =
		height =

		flexParams.field = pi_flexform

		alternativeContent.field = bodytext

		layout = ###SWFOBJECT###

		video {
			player = {$styles.content.media.videoPlayer}

			defaultWidth  = {$styles.content.media.defaultVideoWidth}
			defaultHeight  = {$styles.content.media.defaultVideoHeight}

			default {
				params.quality = high
				params.menu = false
				params.allowScriptAccess = sameDomain
				params.allowFullScreen = true
			}
			mapping {

			}
		}

		audio {
			player = {$styles.content.media.audioPlayer}

			defaultWidth = {$styles.content.media.defaultAudioWidth}
			defaultHeight = {$styles.content.media.defaultAudioHeight}

			default {
				params.quality = high
				params.allowScriptAccess = sameDomain
				params.menu = false
			}
			mapping {
				flashvars.file = soundFile
			}
		}

	}
	20.stdWrap {
		editIcons = tt_content: multimedia, imagewidth, imageheight, pi_flexform, bodytext
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.multimedia

		prefixComment = 2 | SWFobject element:
	}
}

# *****************
# CType: qtobject
# *****************
tt_content.qtobject = COA
tt_content.qtobject {
	10 = < lib.stdheader

	20 = QTOBJECT
	20 {
		file =
		width =
		height =

		flexParams.field = pi_flexform

		alternativeContent.field = bodytext

		layout = ###QTOBJECT###

		video {
			player = {$styles.content.media.videoPlayer}

			defaultWidth  = {$styles.content.media.defaultVideoWidth}
			defaultHeight  = {$styles.content.media.defaultVideoHeight}

			default {
				params.quality = high
				params.menu = false
				params.allowScriptAccess = sameDomain
				params.allowFullScreen = true
			}
			mapping {

			}
		}

		audio {
			player = {$styles.content.media.audioPlayer}

			defaultWidth = {$styles.content.media.defaultAudioWidth}
			defaultHeight = {$styles.content.media.defaultAudioHeight}

			default {
				params.quality = high
				params.allowScriptAccess = sameDomain
				params.menu = false
			}
			mapping {
				flashvars.file = soundFile
			}
		}
	}
	20.stdWrap {
		editIcons = tt_content: multimedia, imagewidth, imageheight, pi_flexform, bodytext
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.multimedia

		prefixComment = 2 | QTobject element:
	}
}

# *****************
# CType: media
# *****************
tt_content.media = COA
tt_content.media {
	10 = < lib.stdheader

	20 = MEDIA
	20 {

		flexParams.field = pi_flexform
		alternativeContent < tt_content.text.20
		alternativeContent.field = bodytext

		type = video
		renderType = auto
		allowEmptyUrl = 0
		forcePlayer = 1

		fileExtHandler {
			default = MEDIA
			avi = MEDIA
			asf = MEDIA
			class = MEDIA
			wmv = MEDIA
			mp3 = SWF
			mp4 = SWF
			m4v = SWF
			swa = SWF
			flv = SWF
			swf = SWF
			mov = QT
			m4v = QT
			m4a = QT
		}

		mimeConf.swfobject < tt_content.swfobject.20
		mimeConf.qtobject < tt_content.qtobject.20

	}
	20.stdWrap {
		editIcons = tt_content: pi_flexform, bodytext
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.multimedia

		prefixComment = 2 | Media element:
	}
}

# ******************
# CType: mailform
# ******************
tt_content.mailform = COA
tt_content.mailform.10 = < lib.stdheader
tt_content.mailform.20 = FORM
tt_content.mailform.20 {
	accessibility = 1
	noWrapAttr=1
	formName = mailform
	dontMd5FieldNames = 1
	layout = <div class="csc-mailform-field">###LABEL### ###FIELD###</div>
	labelWrap.wrap = |
	commentWrap.wrap = |
	radioWrap.wrap = |<br />
	radioWrap.accessibilityWrap = <fieldset###RADIO_FIELD_ID###><legend>###RADIO_GROUP_LABEL###</legend>|</fieldset>
	REQ = 1
	REQ.labelWrap.wrap = |
	COMMENT.layout = <div class="csc-mailform-label">###LABEL###</div>
	RADIO.layout = <div class="csc-mailform-field">###LABEL### <span class="csc-mailform-radio">###FIELD###</span></div>
	LABEL.layout = <div class="csc-mailform-field">###LABEL### <span class="csc-mailform-label">###FIELD###</span></div>
	target = {$styles.content.mailform.target}
	goodMess = {$styles.content.mailform.goodMess}
	badMess = {$styles.content.mailform.badMess}
	redirect.field = pages
	redirect.listNum = 0
	recipient.field = subheader
	data.field = bodytext
	locationData = 1
	hiddenFields.stdWrap.wrap = <div style="display:none;">|</div>

	params.radio = class="csc-mailform-radio"
	params.check = class="csc-mailform-check"
	params.submit = class="csc-mailform-submit"

	stdWrap.wrap = <fieldset class="csc-mailform"> | </fieldset>
	stdWrap {
		editIcons = tt_content: bodytext, pages, subheader
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.form

		prefixComment = 2 | Mail form inserted:
	}
}


# ******************
# CType: search
# ******************
tt_content.search = COA
tt_content.search.10 = < lib.stdheader
# Result:
tt_content.search.20 = SEARCHRESULT
tt_content.search.20 {
	allowedCols = pages.title-subtitle-keywords-description : tt_content.header-bodytext-imagecaption : tt_address.name-title-address-email-company-city-country : tt_links.title-note-note2-url : tt_board.subject-message-author-email : tt_calender.title-note : tt_products.title-note-itemnumber
	languageField.tt_content = sys_language_uid
	renderObj = COA
	renderObj {

		10 = TEXT
		10.field = pages_title
		10.htmlSpecialChars = 1
		10.typolink {
			parameter.field = uid
			target = {$styles.content.searchresult.resultTarget}
			additionalParams.data = register:SWORD_PARAMS
			additionalParams.required = 1
			additionalParams.wrap = &no_cache=1
		}
		10.htmlSpecialChars = 1
		10.wrap = <h3 class="csc-searchResultHeader">|</h3>

		20 = COA
		20 {
			10 = TEXT
			10.field = tt_content_bodytext
			10.stripHtml = 1
			10.htmlSpecialChars = 1
		}
		20.stdWrap.crop = 200 | ...
		20.stdWrap.wrap = <p class="csc-searchResult">|</p>
	}

	layout = COA
	layout {
		wrap = <table border="0" cellspacing="0" cellpadding="2" class="csc-searchResultInfo"><tr> | </tr></table> ###RESULT###

		10 = TEXT
		10.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.resultRange
		10.wrap = <td class="csc-searchResultRange"><p>|</p></td>

		20 = TEXT
		20.value = ###PREV###&nbsp;&nbsp;&nbsp;###NEXT###
		20.wrap = <td class="csc-searchResultPrevNext"><p>|</p></td>
	}

	noResultObj = COA
	noResultObj {
		10 = TEXT
		10.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.emptySearch
		10.wrap = <h3 class="csc-noSearchResultMsg">|</h3>
	}

	next = TEXT
	next.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.searchResultNext

	prev = TEXT
	prev.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.searchResultPrev

	target = {$styles.content.searchresult.target}
	range = 20

	stdWrap.prefixComment = 2 | Search result:
}

# Form:
tt_content.search.30 < tt_content.mailform.20
tt_content.search.30 {
	goodMess = {$styles.content.searchform.goodMess}
	redirect >
	recipient >
	data >
	dataArray {
		10.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.searchWord
		10.type = sword=input
		20.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.searchIn
		20.type = scols=select
		20.valueArray {
			10.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.headersKeywords
			10.value = pages.title-subtitle-keywords-description:tt_content.header
			20.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.pageContent
			20.value = tt_content.header-bodytext-imagecaption
		}
		30.type = stype=hidden
		30.value = L0
		40.type = submit=submit
		40.value.data = LLL:EXT:css_styled_content/pi1/locallang.xml:search.searchButton
	}
	type.field = pages
	type.listNum = 0
	locationData = HTTP_POST_VARS
	no_cache = 1

	stdWrap.wrap = <table border="0" cellspacing="1" cellpadding="1" class="csc-searchform">  | </table>
	stdWrap {
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.search

		prefixComment = 2 | Search form inserted:
	}
}


# ******************
# CType: login
# ******************
tt_content.login < tt_content.mailform
tt_content.login.10 = < lib.stdheader
tt_content.login.20 {
	goodMess = {$styles.content.loginform.goodMess}
	redirect >
	recipient >
	data >
	dataArray {
		10.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:login.username
		10.type = *user=input
		20.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:login.password
		20.type = *pass=password
		30.type = logintype=hidden
		30.value = login
		40.type = submit=submit
		40.value.data = LLL:EXT:css_styled_content/pi1/locallang.xml:login.login
	}
	type.field = pages
	type.listNum = 0
	target = {$styles.content.loginform.target}
	locationData = 0
	hiddenFields.pid = TEXT
	hiddenFields.pid {
		value = {$styles.content.loginform.pid}
		override.field = pages
		override.listNum = 1
	}

	stdWrap.wrap = <div class="csc-loginform"> | </div>
	stdWrap {
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.login

		prefixComment = 2 | Login/Logout form:
	}
}
[loginUser = *]
tt_content.login.20 {
	dataArray >
	dataArray {
		10.label.data = LLL:EXT:css_styled_content/pi1/locallang.xml:login.username
		10.label.wrap = |&nbsp;<!--###USERNAME###-->
		30.type = logintype=hidden
		30.value = logout
		40.type = submit=submit
		40.value.data = LLL:EXT:css_styled_content/pi1/locallang.xml:login.logout
	}
}
[global]


# ******************
# CType: splash
# ******************
# Deprecated element.
# Still here for backwards compliance with plugins using the "text box" type.
tt_content.splash = CASE
tt_content.splash.key.field = splash_layout
tt_content.splash.stdWrap {
	prefixComment = 2 | Textbox inserted (Deprecated)
}
tt_content.splash.default = COA
tt_content.splash.default {
	20 = CTABLE
	20 {
		c.1 = < tt_content.text
		lm.1 = IMAGE
		lm.1.file {
			import = uploads/pics/
			import.field = image
			import.listNum = 0
			maxW.field = imagewidth
			maxW.ifEmpty = 200
		}
		cMargins = 30,0,0,0
	}
}
tt_content.splash.1 < tt_content.splash.default
tt_content.splash.1.20.lm.1.file >
tt_content.splash.1.20.lm.1.file = GIFBUILDER
tt_content.splash.1.20.lm.1.file {
	XY = [10.w]+10,[10.h]+10
	backColor = {$content.splash.bgCol}
	backColor.override.data = register:pageColor
	format = jpg
	5 = BOX
	5.dimensions = 3,3,[10.w],[10.h]
	5.color = #333333
	7 = EFFECT
	7.value = blur=99|blur=99|blur=99|blur=99|blur=99|blur=99|blur=99
	10 = IMAGE
	10.file {
		import = uploads/pics/
		import.field = image
		import.listNum = 0
		maxW.field = imagewidth
		maxW.ifEmpty = 200
	}
}
// The image frames are not available unless TypoScript code from styles.content.imgFrames.x is provided manually:
tt_content.splash.2 < tt_content.splash.default
#tt_content.splash.2.20.lm.1.file.m < styles.content.imgFrames.1
tt_content.splash.3 < tt_content.splash.default
#tt_content.splash.3.20.lm.1.file.m < styles.content.imgFrames.2

// From plugin.postit1, if included:
tt_content.splash.20 = < plugin.postit1



# ****************
# CType: menu
# ****************
tt_content.menu = COA
tt_content.menu {
	10 = < lib.stdheader

	20 = CASE
	20 {
		key.field = menu_type

		# "Menu of these pages"
		default = HMENU
		default {
			special = list
			special.value.field = pages
			wrap = <ul class="csc-menu csc-menu-def">|</ul>
			1 = TMENU
			1 {
				target = {$PAGE_TARGET}
				NO {
					stdWrap.htmlSpecialChars = 1
					wrapItemAndSub = <li>|</li>
					ATagTitle.field = description // title
				}
				noBlur = 1
			}
		}

		# "Menu of subpages to these pages"
		1 < .default
		1 {
			special = directory
			wrap = <ul class="csc-menu csc-menu-1">|</ul>
		}

		# "Sitemap - liststyle"
		2 = HMENU
		2 {
			wrap = <div class="csc-sitemap">|</div>
			1 = TMENU
			1 {
				target = {$PAGE_TARGET}
				noBlur = 1
				expAll = 1
				wrap = <ul>|</ul>
				NO {
					stdWrap.htmlSpecialChars = 1
					wrapItemAndSub = <li>|</li>
					ATagTitle.field = description // title
				}
			}
			2 < .1
			3 < .1
			4 < .1
			5 < .1
			6 < .1
			7 < .1
		}

		# "Section index (pagecontent w/Index checked - liststyle)"
		3 < styles.content.get
		3 {
			wrap = <ul class="csc-menu csc-menu-3">|</ul>
			select.andWhere = sectionIndex!=0
			select.pidInList.override.field = pages
			renderObj = TEXT
			renderObj {
				fieldRequired = header
				trim = 1
				field = header
				htmlSpecialChars = 1
				noBlur = 1
				wrap = <li class="csc-section">|</li>
				typolink.parameter.field = pid
				typolink.section.field = uid
			}
		}

		# "Menu of subpages to these pages (with abstract)"
		4 < .1
		4 {
			wrap = <dl class="csc-menu csc-menu-4">|</dl>
			1.NO {
				wrapItemAndSub >
				linkWrap = <dt>|</dt>
				after {
					data = field : abstract // field : description // field : subtitle
					required = 1
					htmlSpecialChars = 1
					wrap = <dd>|</dd>
				}
				ATagTitle.field = description // title
			}
		}

		# "Recently updated pages"
		5 < .default
		5 {
			wrap = <ul class="csc-menu csc-menu-5">|</ul>
			special = updated
			special {
				maxAge = 3600*24*7
				excludeNoSearchPages = 1
			}
		}

		# "Related pages (based on keywords)"
		6 < .default
		6 {
			wrap = <ul class="csc-menu csc-menu-6">|</ul>
			special = keywords
			special {
				excludeNoSearchPages = 1
			}
		}

		# "Menu of subpages to these pages + sections - liststyle"
		7 < .1
		7 {
			wrap = <ul class="csc-menu csc-menu-7">|</ul>
			1.expAll = 1
			2 < .1
			2 {
				sectionIndex = 1
				sectionIndex.type = header
				wrap = <ul>|</ul>
				NO.wrapItemAndSub = <li class="csc-section">|</li>
			}
		}
	}

	20.stdWrap {
		editIcons = tt_content: menu_type, pages
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.menuSitemap

		prefixComment = 2 | Menu/Sitemap element:
	}
}



# ****************
# CType: shortcut
# ****************
# Should be a complete copy from the old static template "content (default)"
tt_content.shortcut = COA
tt_content.shortcut {
	20 = CASE
	20.key.field = layout
	20.0= RECORDS
	20.0 {
		source.field = records
		tables = {$content.shortcut.tables}
		# THESE are OLD plugins. Modern plugins registers themselves automatically!
		conf.tt_content = < tt_content
		conf.tt_address = < tt_address
		conf.tt_links = < tt_links
		conf.tt_guest = < tt_guest
		conf.tt_board = < tt_board
		conf.tt_calender = < tt_calender
		conf.tt_rating < tt_rating
		conf.tt_products = < tt_products
		conf.tt_news = < tt_news
		conf.tt_poll = < plugin.tt_poll
	}
	20.1= RECORDS
	20.1 {
		source.field = records
		tables = {$content.shortcut.tables}
		conf.tt_poll = < plugin.tt_poll
		conf.tt_poll.code = RESULT,SUBMITTEDVOTE
	}

	20.stdWrap {
		editIcons = tt_content: records
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.recordList

		prefixComment = 2 | Inclusion of other records (by reference):
	}
}


# ****************
# CType: list
# ****************
# Should be a complete copy from the old static template "content (default)" (except "lib.stdheader")
tt_content.list = COA
tt_content.list {
	10 = < lib.stdheader

	20 = CASE
	20.key.field = list_type
	20 {
		# LIST element references (NOT copy of objects!)
		# THESE are OLD plugins. Modern plugins registers themselves automatically!
		3 = CASE
		3.key.field = layout
		3.0 = < plugin.tt_guest

		4 = CASE
		4.key.field = layout
		4.0 = < plugin.tt_board_list
		4.1 = < plugin.tt_board_tree

		2 = CASE
		2.key.field = layout
		2.0 = < plugin.tt_board_tree

		5 = CASE
		5.key.field = layout
		5.0 = < plugin.tt_products

		7 = CASE
		7.key.field = layout
		7.0 = < plugin.tt_calender

		8 = CASE
		8.key.field = layout
		8.0 = < plugin.tt_rating

		9 = CASE
		9.key.field = layout
		9.0 = < plugin.tt_news

		11 = CASE
		11.key.field = layout
		11.0 = < plugin.tipafriend

		20 = CASE
		20.key.field = layout
		20.0 = < plugin.feadmin.fe_users

		21 = CASE
		21.key.field = layout
		21.0 = < plugin.feadmin.dmailsubscription
	}

	20.stdWrap {
		editIcons = tt_content: list_type, layout, select_key, pages [recursive]
		editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.plugin

		prefixComment = 2 | Plugin inserted:
	}
}


# ****************
# CType: script
# ****************
# OBSOLETE! Please make extensions instead. The "script" content element was meant for these custom purposes in the past. Today extensions will do the job better.
tt_content.script = TEXT
tt_content.script {
	value =

	prefixComment = 2 | Script element (Deprecated)
}


# ****************
# CType: div
# ****************
tt_content.div = TEXT
tt_content.div {
	value = <hr />
	wrap = <div class="divider">|</div>
	prefixComment = 2 | Div element
}


# ****************
# CType: html
# ****************
# This truely IS a content object, launched from inside the PHP class of course.
# Should be a complete copy from the old static template "content (default)"
tt_content.html = TEXT
tt_content.html {
	field = bodytext

	editIcons = tt_content: pages
	editIcons.iconTitle.data = LLL:EXT:css_styled_content/pi1/locallang.xml:eIcon.html

	prefixComment = 2 | Raw HTML content:
}


# ****************
# Default error msg:
# ****************
tt_content.default = TEXT
tt_content.default {
	field = CType
	wrap = <p style="background-color: yellow;"><b>ERROR:</b> Content Element type "|" has no rendering definition!</p>

	prefixComment = 2 | Unknown element message:
}

# *********************************************************************
# ACCESSIBILTY MODE
# *********************************************************************







plugin.tx_cssstyledcontent._CSS_DEFAULT_STYLE (
	/* Captions */
	DIV.csc-textpic-caption-c .csc-textpic-caption { text-align: center; }
	DIV.csc-textpic-caption-r .csc-textpic-caption { text-align: right; }
	DIV.csc-textpic-caption-l .csc-textpic-caption { text-align: left; }

	/* Needed for noRows setting */
	DIV.csc-textpic DIV.csc-textpic-imagecolumn { float: left; display: inline; }

	/* Border just around the image */
	{$styles.content.imgtext.borderSelector} {
		border: {$styles.content.imgtext.borderThick}px solid {$styles.content.imgtext.borderColor};
		padding: {$styles.content.imgtext.borderSpace}px {$styles.content.imgtext.borderSpace}px;
	}

	DIV.csc-textpic-imagewrap { padding: 0; }

	DIV.csc-textpic IMG { border: none; }

	/* DIV: This will place the images side by side */
	DIV.csc-textpic DIV.csc-textpic-imagewrap DIV.csc-textpic-image { float: left; }

	/* UL: This will place the images side by side */
	DIV.csc-textpic DIV.csc-textpic-imagewrap UL { list-style: none; margin: 0; padding: 0; }
	DIV.csc-textpic DIV.csc-textpic-imagewrap UL LI { float: left; margin: 0; padding: 0; }

	/* DL: This will place the images side by side */
	DIV.csc-textpic DIV.csc-textpic-imagewrap DL.csc-textpic-image { float: left; }
	DIV.csc-textpic DIV.csc-textpic-imagewrap DL.csc-textpic-image DT { float: none; }
	DIV.csc-textpic DIV.csc-textpic-imagewrap DL.csc-textpic-image DD { float: none; }
	DIV.csc-textpic DIV.csc-textpic-imagewrap DL.csc-textpic-image DD IMG { border: none; } /* FE-Editing Icons */
	DL.csc-textpic-image { margin: 0; }
	DL.csc-textpic-image DT { margin: 0; display: inline; }
	DL.csc-textpic-image DD { margin: 0; }

	/* Clearer */
	DIV.csc-textpic-clear { clear: both; }

	/* Margins around images: */

	/* Pictures on left, add margin on right */
	DIV.csc-textpic-left DIV.csc-textpic-imagewrap .csc-textpic-image,
	DIV.csc-textpic-intext-left-nowrap DIV.csc-textpic-imagewrap .csc-textpic-image,
	DIV.csc-textpic-intext-left DIV.csc-textpic-imagewrap .csc-textpic-image {
		display: inline; /* IE fix for double-margin bug */
		margin-right: {$styles.content.imgtext.colSpace}px;
	}

	/* Pictures on right, add margin on left */
	DIV.csc-textpic-right DIV.csc-textpic-imagewrap .csc-textpic-image,
	DIV.csc-textpic-intext-right-nowrap DIV.csc-textpic-imagewrap .csc-textpic-image,
	DIV.csc-textpic-intext-right DIV.csc-textpic-imagewrap .csc-textpic-image {
		display: inline; /* IE fix for double-margin bug */
		margin-left: {$styles.content.imgtext.colSpace}px;
	}

	/* Pictures centered, add margin on left */
	DIV.csc-textpic-center DIV.csc-textpic-imagewrap .csc-textpic-image {
		display: inline; /* IE fix for double-margin bug */
		margin-left: {$styles.content.imgtext.colSpace}px;
	}
	DIV.csc-textpic DIV.csc-textpic-imagewrap .csc-textpic-image .csc-textpic-caption { margin: 0; }
	DIV.csc-textpic DIV.csc-textpic-imagewrap .csc-textpic-image IMG { margin: 0; vertical-align:bottom; }

	/* Space below each image (also in-between rows) */
	DIV.csc-textpic DIV.csc-textpic-imagewrap .csc-textpic-image { margin-bottom: {$styles.content.imgtext.rowSpace}px; }
	DIV.csc-textpic-equalheight DIV.csc-textpic-imagerow { margin-bottom: {$styles.content.imgtext.rowSpace}px; display: block; }
	DIV.csc-textpic DIV.csc-textpic-imagerow { clear: both; }
	DIV.csc-textpic DIV.csc-textpic-single-image IMG { margin-bottom: {$styles.content.imgtext.rowSpace}px; }

	/* IE7 hack for margin between image rows */
	*+html DIV.csc-textpic DIV.csc-textpic-imagerow .csc-textpic-image { margin-bottom: 0; }
	*+html DIV.csc-textpic DIV.csc-textpic-imagerow { margin-bottom: {$styles.content.imgtext.rowSpace}px; }

	/* No margins around the whole image-block */
	DIV.csc-textpic DIV.csc-textpic-imagewrap .csc-textpic-firstcol { margin-left: 0px !important; }
	DIV.csc-textpic DIV.csc-textpic-imagewrap .csc-textpic-lastcol { margin-right: 0px !important; }

	/* Add margin from image-block to text (in case of "Text w/ images") */
	DIV.csc-textpic-intext-left DIV.csc-textpic-imagewrap,
	DIV.csc-textpic-intext-left-nowrap DIV.csc-textpic-imagewrap {
		margin-right: {$styles.content.imgtext.textMargin}px !important;
	}
	DIV.csc-textpic-intext-right DIV.csc-textpic-imagewrap,
	DIV.csc-textpic-intext-right-nowrap DIV.csc-textpic-imagewrap {
		margin-left: {$styles.content.imgtext.textMargin}px !important;
	}

	/* Positioning of images: */

	/* Above */
	DIV.csc-textpic-above DIV.csc-textpic-text { clear: both; }

	/* Center (above or below) */
	DIV.csc-textpic-center { text-align: center; /* IE-hack */ }
	DIV.csc-textpic-center DIV.csc-textpic-imagewrap { margin: 0 auto; }
	DIV.csc-textpic-center DIV.csc-textpic-imagewrap .csc-textpic-image { text-align: left; /* Remove IE-hack */ }
	DIV.csc-textpic-center DIV.csc-textpic-text { text-align: left; /* Remove IE-hack */ }

	/* Right (above or below) */
	DIV.csc-textpic-right DIV.csc-textpic-imagewrap { float: right; }
	DIV.csc-textpic-right DIV.csc-textpic-text { clear: right; }

	/* Left (above or below) */
	DIV.csc-textpic-left DIV.csc-textpic-imagewrap { float: left; }
	DIV.csc-textpic-left DIV.csc-textpic-text { clear: left; }

	/* Left (in text) */
	DIV.csc-textpic-intext-left DIV.csc-textpic-imagewrap { float: left; }

	/* Right (in text) */
	DIV.csc-textpic-intext-right DIV.csc-textpic-imagewrap { float: right; }

	/* Right (in text, no wrap around) */
	DIV.csc-textpic-intext-right-nowrap DIV.csc-textpic-imagewrap { float: right; clear: both; }
	/* Hide from IE5-mac. Only IE-win sees this. \*/
	* html DIV.csc-textpic-intext-right-nowrap .csc-textpic-text { height: 1%; }
	/* End hide from IE5/mac */

	/* Left (in text, no wrap around) */
	DIV.csc-textpic-intext-left-nowrap DIV.csc-textpic-imagewrap { float: left; clear: both; }
	/* Hide from IE5-mac. Only IE-win sees this. \*/
	* html DIV.csc-textpic-intext-left-nowrap .csc-textpic-text,
	* html .csc-textpic-intext-left ol,
	* html .csc-textpic-intext-left ul { height: 1%; }
	/* End hide from IE5/mac */

	DIV.csc-textpic DIV.csc-textpic-imagerow-last { margin-bottom: 0; }

	/* Browser fixes: */

	/* Fix for unordered and ordered list with image "In text, left" */
	.csc-textpic-intext-left ol, .csc-textpic-intext-left ul {padding-left: 40px; overflow: auto; }
)

# TYPO3 SVN ID: $Id$

