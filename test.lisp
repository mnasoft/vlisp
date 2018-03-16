;;;; test.lisp

(in-package #:vlisp)

;;;;  (load-vlisp-file "./src/lsp/utils/draw.lsp")
(progn 
  (load-vlisp-file "./bin/Axis.VLX")
  (load-vlisp-file "./bin/dim_style.VLX")
  (load-vlisp-file "./bin/lines.VLX")
  
  (axis-block-scale-set 2.5)
  (axis-load-point-types)
  (dr-point        '(10 20 30) 256 )
  (dr-ch_prop      '((62 . 5)))
  (dr-points       '((0 0 0) (10 10 10 )) 1)
  (dr-pline        '((0 0 0) (10 10 10 )) 1)
  (dr-axis         '(0 0 0) '(0 100 0) 0 100 0 "COOL_axis")
  (dr-line         '( 0  0) '(100 100) 6)
  (dr-circle       '(20 30)   25.3 2)
  (dr-arc          '(30 20)   25.3 pi (* 3/4 pi)  2 )
  (dr-xline        '( 5 15) '(30 20) 3)
  (dr-ray          '( 5 15) '(30 20) 4)
  (dr-text          "New text !!!" '( 5 15) 4.5 0.0 54  :alignment 7 )
  (dr-solid        '( 5 15) '(30 20) '( 5 -15) '(-30 20) 56)
  (dr-spline      '(( 5 15) (30 20) ( 5 -15) (-30 20)) 86)
  (dr-layer-new "New-layer")
  (dr-layer-set "New-layer")
  (dr-insert '(56 89) "123" 1.0 1.0 1.0 0.0)
  (dr-mtext "Sample text sample text sample text sample text sample text sample text sample text sample text " '(10 10 ) 100 3.15 0.0 54 2)
  )

(defun test-axis-draw-points ()
  (load-vlisp-file "./bin/Axis.VLX")
  (load-vlisp-file "./bin/dim_style.VLX")
  (axis-load-reset-point-types)
  (let ((start-point (vector+ (mid-point (vector+ *origin* '(20 5 0))
					 (vector- '(210 297 0) '(5 5 0)))
			      '(-50 287/4 0)))
	(delta-x 10)
	(delta-y 10)
	(scale 2.0))
    (dr-format-a4 '(0 0 0))
    (dotimes (j 5)
      (dotimes (i 10)
	(axis-point-type-next)
	(dr-insert
	 (mapcar #'+ start-point (list (* i delta-x) (* j delta-y) 0))
	 (axis-point-type-block-name :os nil) scale scale scale 0.0
	 :format-sting "(dr:insert '(~{~f~^ ~}) ~a ~f ~f ~f ~f)~%")))))

(test-axis-draw-points) 

(defun test-line ()
  (load-vlisp-file "./bin/Axis.VLX")
  (load-vlisp-file "./bin/lines.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (lines-load-line-types)
  (let* ((lt-lst (list "Continuous"
		       "CENTER" "CENTER2" "CENTER4" "CENTERX2" "CENTERX4"
		       "HIDDEN" "HIDDEN2" "HIDDEN4" "HIDDENX2" "HIDDENX4"
		       "DASHDOT" "DASHDOT2" "DASHDOT4" "DASHDOTX2" "DASHDOTX4"))
	 (length 150)
	 (delta-y 10)
	 (start-point (vector+ (list (/ length -2) -50 0 )
			       (mid-point (vector+ *origin* '(20 5 0))
					  (vector+ *origin* '(205 292 0))))))
    (mapcar #'(lambda (lt)
		(setvar "CELTYPE" lt)
		(dr-line start-point (polar start-point 0 length) 256)
		(setf start-point (vector+ start-point (list 0  delta-y 0))))
	    lt-lst)))

(test-line)

(defun test-point ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((start-point (mid-point (vector+ *origin* '(20 5 0))
					 (vector+ *origin* '(205 292 0))))
	(x-max 150)
	(y-max 200))
  (dotimes (i 100)
    (dr-point (vector+ start-point
			  (vector- (list (random (1+ x-max)) (random (1+ y-max)) 0)
				   (list (/ x-max 2) (/ y-max 2) 0)))
	      (1+ (random 7))))
  (setvar "PDMODE" 35)))

(test-point)

(defun test-points ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((start-point (mid-point (vector+ *origin* '(20 5 0))
				(vector+ *origin* '(205 292 0))))
	(x-max 150)
	(y-max 200)
	(pts nil))
    (dotimes (i 100)
      (push (vector+ start-point
		     (vector- (list (random (1+ x-max)) (random (1+ y-max)) 0)
			      (list (/ x-max 2) (/ y-max 2) 0)))
	    pts))
    (dr-points pts (1+ (random 7)))
    (setvar "PDMODE" 35)))

(test-points)

(defun test-arc-circle ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((left-point '(-20 0 0))
	(right-point '(20 0 0))
	(start-point (mid-point (vector+ *origin* '(20 5 0))
				(vector+ *origin* '(205 292 0))))
	(r-min 20)
	(delta-r 5))
    (dotimes (i 8)
      (dr-circle (vector+ start-point left-point ) (+ r-min (* i delta-r))   (1+ (random 7)))
      (dr-arc (vector+ start-point right-point) (+ r-min (* i delta-r)) (/ (random 10000) 1000) (/ (random 10000) 1000)  (1+ (random 7))))))

(test-arc-circle)

(defun test-axis ()
  (load-vlisp-file "./bin/Axis.VLX")
  (setvar "CELTYPE" "Continuous")
  (dr-format-a4 '(0 0 0))
  (let ((dy   '(40 135) )
	(dx   '(40 110) )
	)
    (dr-axis (vector+ dx  '(0 0)) (vector+ dx '(150 0))     0 10 0 "x")
    (dr-axis (vector+ dy  '(0 0)) (vector+ dy '( 0 150))    0 10 0 "x1")
    (dr-axis (vector+ dy '(-5 0)) (vector+ dy '(-5 150))    0 10 0 "x2")
    (dr-axis (vector+ dy '(-10 0)) (vector+ dy '(-10 150))  0 8 0 "x3")
    (axis-draw-pline-set nil)
					;    (axis-draw-spline-set nil)
    (axis-draw-point-set t)
    (axis-draw-multiple-graphs-by-axis-names
     "x" '(1 2 3 4 5 6 7 8 9)
     '("x1" "x2" "x3")
     '((0.1 0.4 0.9 1.6 2.5 3.6 4.9 6.4 8.1)
       (0.01 0.08 0.27 0.64 1.25 2.16 3.43 5.12 7.29)
       (0.1 0.4 0.9 1.6 2.5 3.6 4.9 6.4 8.1)))))

(test-axis)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(vl-load-com)
(defun c-ADD_PROPS (/ doc db si author nc nc2 nc3 value3 value4)
  (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (setq db (vla-get-Database doc))
  (setq si (vla-get-SummaryInfo db))

  (vla-put-author si "John")
  (vla-put-comments si "New comments")
  (vla-put-hyperlinkbase si "http://AddURL")
  (vla-put-keywords si "New keywords")

  (vla-AddCustomInfo si "siPutKey" "siPutValue")

  (setq nc (vla-numcustominfo si))
  (vla-SetCustomByKey si "siPutKey" "siPutValueByKey")
  (vla-GetCustomByKey si "siPutKey" 'value3)

  (if (/= "siPutValueByKey" value3)
      (princ "*** Error SetCustomByKey\n")
      )

  (vla-SetCustomByIndex si (1- nc) "siPutCustomByIndexKey" "siPutCustomByIndexValue")
  (vla-GetCustomByKey si "siPutCustomByIndexKey" 'value4)

  (if (/= "siPutCustomByIndexValue" value4)
      (princ "*** Error SetCustomByIndex\n")
      )

  (vla-RemoveCustomByIndex si (1- nc))

  (setq nc2 (vla-numcustominfo si))
  (if (/= nc2 (1- nc))
      (princ "*** Error RemoveCustomByIndex")
      )

  (vla-AddCustomInfo si "siPutKey" "siPutValue")

					; Remove property
  (vla-RemoveCustomByKey si "siPutKey")
  (setq nc3 (vla-numcustominfo si))
  (if (/= nc2 (1- nc))
      (princ "*** Error RemoveCustomByKey")
      )

  (vla-AddCustomInfo si "siPutKey" "siPutValue")

  (vlax-release-object si)
  (vlax-release-object db)
  (vlax-release-object doc)
  (princ)
  )
(princ)

(vl-load-com)
(defun c:GET_PROPS (/ doc db si author )
  (if (/= "MyDrawing.dwg" (getvar "DWGNAME"))
    (princ "Open MyDrawing.dwg")
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-Acad-Object)))
      (setq db (vla-get-Database doc))
      (setq si (vla-get-SummaryInfo db))

      (princ "\nAuthor: \n")
      (if (/= "John" (setq author (vla-get-author si)))
        (princ "*** vla-get-author error")
        (princ author)
      )
      (princ "\n")
      (princ "\nComments:\n ")
      (princ (vla-get-comments si))
      (princ "\n")
      (princ "\nHyperlink-base: \n")
      (princ (vla-get-HyperlinkBase si))
      (princ "\n")
      (princ "\nNumber of custom properties: ")
      (princ (setq nc (vla-numcustominfo si)))
      (princ "\n")
      (while (> nc 0)
        (princ "Custom property ")
        (princ nc)
        (vla-GetCustomByIndex si (- nc 1) 'key 'value)
        (princ ": key(")
        (princ key)
        (princ ")")
        (princ " value(")
        (princ value)
        (princ ")\n")
        (vla-GetCustomByKey si key 'value2)
        (if (/= value value2)
          (princ "\n*** Error GetCustomByKey returned unexpected result.\n")
        )
        (setq nc (1- nc))
      )
      (vlax-release-object si)
      (vlax-release-object db)
      (vlax-release-object doc)
    )
  )
 (princ)
)


vla-Activate
vla-Add
vla-Add3DFace
vla-Add3DMesh
vla-Add3DPoly
vla-AddArc
vla-AddAttribute
vla-AddBox
vla-AddCircle
vla-AddCone
vla-AddCustomInfo
vla-AddCustomObject
vla-AddCylinder
vla-AddDim3PointAngular
vla-AddDimAligned
vla-AddDimAngular
vla-AddDimArc
vla-AddDimDiametric
vla-AddDimOrdinate
vla-AddDimRadial
vla-AddDimRadialLarge
vla-AddDimRotated
vla-AddEllipse
vla-AddEllipticalCone
vla-AddEllipticalCylinder
vla-AddExtrudedSolid
vla-AddExtrudedSolidAlongPath
vla-AddFitPoint
vla-AddHatch
vla-AddItems
vla-AddLeader
vla-AddLeaderLine
vla-AddLeaderLineEx
vla-AddLightWeightPolyline
vla-AddLine
vla-AddMenuItem
vla-AddMInsertBlock
vla-AddMLeader
vla-AddMLine
vla-AddMText
vla-AddObject
vla-AddPoint
vla-AddPolyfaceMesh
vla-AddPolyline
vla-AddPViewport
vla-AddRaster
vla-AddRay
vla-AddRegion
vla-AddRevolvedSolid
vla-AddSection
vla-AddSeparator
vla-AddShape
vla-AddSolid
vla-AddSphere
vla-AddSpline
vla-AddSubMenu
vla-AddTable
vla-AddText
vla-AddTolerance
vla-AddToolbarButton
vla-AddTorus
vla-AddTrace
vla-AddVertex
vla-AddWedge
vla-AddXline
vla-AddXRecord
vla-AngleFromXAxis
vla-AngleToReal
vla-AngleToString
vla-AppendInnerLoop
vla-AppendItems
vla-AppendOuterLoop
vla-AppendVertex
vla-ArrayPolar
vla-ArrayRectangular
vla-AttachExternalReference
vla-AttachToolbarToFlyout
vla-AuditInfo

vla-Bind
vla-Block
vla-Boolean

vla-CheckInterference
vla-Clear
vla-ClearSubSelection
vla-ClearTableStyleOverrides
vla-ClipBoundary
vla-Close
vla-ConvertToAnonymousBlock
vla-ConvertToStaticBlock
vla-Copy
vla-CopyFrom
vla-CopyObjects
vla-CopyProfile
vla-CreateCellStyle
vla-CreateCellStyleFromStyle
vla-CreateContent
vla-CreateJog
vla-CreateTypedArray

vla-Delete
vla-DeleteCellContent
vla-DeleteCellStyle
vla-DeleteColumns
vla-DeleteConfiguration
vla-DeleteContent
vla-DeleteFitPoint
vla-DeleteProfile
vla-DeleteRows
vla-Detach
vla-Display
vla-DisplayPlotPreview
vla-DistanceToReal
vla-Dock

vla-ElevateOrder
vla-EnableMergeAll
vla-EndUndoMark
vla-Erase
vla-Eval
vla-Evaluate
vla-Explode
vla-Export
vla-ExportProfile

vla-FieldCode
vla-Float
vla-FormatValue

vla-GenerateLayout
vla-GenerateSectionGeometry
vla-GenerateUsageData
vla-get-Action
vla-get-Active
vla-get-ActiveDimStyle
vla-get-ActiveDocument
vla-get-ActiveInvProject
vla-get-ActiveLayer
vla-get-ActiveLayout
vla-get-ActiveLinetype
vla-get-ActiveMaterial
vla-get-ActiveProfile
vla-get-ActivePViewport
vla-get-ActiveSelectionSet
vla-get-ActiveSpace
vla-get-ActiveTextStyle
vla-get-ActiveUCS
vla-get-ActiveViewport
vla-get-ADCInsertUnitsDefaultSource
vla-get-ADCInsertUnitsDefaultTarget
vla-get-AdjustForBackground
vla-get-Algorithm
vla-get-Alignment
vla-get-AlignmentPointAcquisition
vla-get-AlignSpace
vla-get-AllowedValues
vla-get-AllowLongSymbolNames
vla-get-AllowManualHeights
vla-get-AllowManualPositions
vla-get-AltFontFile
vla-get-Altitude
vla-get-AltRoundDistance
vla-get-AltSubUnitsFactor
vla-get-AltSubUnitsSuffix
vla-get-AltSuppressLeadingZeros
vla-get-AltSuppressTrailingZeros
vla-get-AltSuppressZeroFeet
vla-get-AltSuppressZeroInches
vla-get-AltTabletMenuFile
vla-get-AltTextPrefix
vla-get-AltTextSuffix
vla-get-AltTolerancePrecision
vla-get-AltToleranceSuppressLeadingZeros
vla-get-AltToleranceSuppressTrailingZeros
vla-get-AltToleranceSuppressZeroFeet
vla-get-AltToleranceSuppressZeroInches
vla-get-AltUnits
vla-get-AltUnitsFormat
vla-get-AltUnitsPrecision
vla-get-AltUnitsScale
vla-get-angle
vla-get-AngleFormat
vla-get-AngleVertex
vla-get-Annotation
vla-get-Annotative
vla-get-Application
vla-get-ArcEndParam
vla-get-ArcLength
vla-get-ArcPoint
vla-get-ArcSmoothness
vla-get-ArcStartParam
vla-get-Area
vla-get-Arrowhead1Block
vla-get-Arrowhead1Type
vla-get-Arrowhead2Block
vla-get-Arrowhead2Type
vla-get-ArrowheadBlock
vla-get-ArrowheadSize
vla-get-ArrowheadType
vla-get-ArrowSize
vla-get-ArrowSymbol
vla-get-AssociativeHatch
vla-get-AttachmentPoint
vla-get-Author
vla-get-AutoAudit
vla-get-AutomaticPlotLog
vla-get-AutoSaveInterval
vla-get-AutoSavePath
vla-get-AutoSnapAperture
vla-get-AutoSnapApertureSize
vla-get-AutoSnapMagnet
vla-get-AutoSnapMarker
vla-get-AutoSnapMarkerColor
vla-get-AutoSnapMarkerSize
vla-get-AutoSnapToolTip
vla-get-AutoTrackingVecColor
vla-get-AutoTrackTooltip
vla-get-AxisDirection
vla-get-AxisPosition
vla-get-BackgroundColor
vla-get-BackgroundFill
vla-get-BackgroundLinesColor
vla-get-BackgroundLinesHiddenLine
vla-get-BackgroundLinesLayer
vla-get-BackgroundLinesLinetype
vla-get-BackgroundLinesLinetypeScale
vla-get-BackgroundLinesLineweight
vla-get-BackgroundLinesPlotStyleName
vla-get-BackgroundLinesVisible
vla-get-Backward
vla-get-Bank
vla-get-BasePoint
vla-get-BaseRadius
vla-get-BatchPlotProgress
vla-get-BeepOnError
vla-get-BigFontFile
vla-get-BitFlags
vla-get-Block
vla-get-BlockColor
vla-get-BlockConnectionType
vla-get-BlockRotation
vla-get-Blocks
vla-get-BlockScale
vla-get-BlockScaling
vla-get-Blue
vla-get-BookName
vla-get-BottomHeight
vla-get-BreaksEnabled
vla-get-BreakSize
vla-get-BreakSpacing
vla-get-Brightness
vla-get-CanonicalMediaName
vla-get-Caption
vla-get-CategoryName
vla-get-Center
vla-get-CenterMarkSize
vla-get-CenterPlot
vla-get-CenterPoint
vla-get-CenterType
vla-get-Centroid
vla-get-Check
vla-get-ChordPoint
vla-get-Circumference
vla-get-Clipped
vla-get-ClippingEnabled
vla-get-Closed
vla-get-Closed2
vla-get-Color
vla-get-ColorBookPath
vla-get-ColorIndex
vla-get-ColorMethod
vla-get-ColorName
vla-get-ColorScheme
vla-get-Columns
vla-get-ColumnSpacing
vla-get-CommandDisplayName
vla-get-Comment
vla-get-Comments
vla-get-ConfigFile
vla-get-ConfigName
vla-get-Constant
vla-get-ConstantWidth
vla-get-Constrain
vla-get-ContentBlockName
vla-get-ContentBlockType
vla-get-ContentType
vla-get-ContinuousPlotLog
vla-get-ContourLinesPerSurface
vla-get-Contrast
vla-get-ControlPoints
vla-get-Coordinate
vla-get-Coordinates
vla-get-Count
vla-get-CreaseLevel
vla-get-CreaseType
vla-get-CreateBackup
vla-get-CurrentSectionType
vla-get-CursorSize
vla-get-CurveTangencyLinesColor
vla-get-CurveTangencyLinesLayer
vla-get-CurveTangencyLinesLinetype
vla-get-CurveTangencyLinesLinetypeScale
vla-get-CurveTangencyLinesLineweight
vla-get-CurveTangencyLinesPlotStyleName
vla-get-CurveTangencyLinesVisible
vla-get-CustomDictionary
vla-get-CustomIconPath
vla-get-CustomScale
vla-get-CvHullDisplay
vla-get-Database
vla-get-DecimalSeparator
vla-get-DefaultInternetURL
vla-get-DefaultOutputDevice
vla-get-DefaultPlotStyleForLayer
vla-get-DefaultPlotStyleForObjects
vla-get-DefaultPlotStyleTable
vla-get-DefaultPlotToFilePath
vla-get-Degree
vla-get-Degree2
vla-get-Delta
vla-get-DemandLoadARXApp
vla-get-Description
vla-get-DestinationBlock
vla-get-DestinationFile
vla-get-Diameter
vla-get-Dictionaries
vla-get-DimConstrDesc
vla-get-DimConstrExpression
vla-get-DimConstrForm
vla-get-DimConstrName
vla-get-DimConstrReference
vla-get-DimConstrValue
vla-get-DimensionLineColor
vla-get-DimensionLineExtend
vla-get-DimensionLinetype
vla-get-DimensionLineWeight
vla-get-DimLine1Suppress
vla-get-DimLine2Suppress
vla-get-DimLineInside
vla-get-DimLineSuppress
vla-get-DimStyles
vla-get-DimTxtDirection
vla-get-Direction
vla-get-DirectionVector
vla-get-Display
vla-get-DisplayGrips
vla-get-DisplayGripsWithinBlocks
vla-get-DisplayLayoutTabs
vla-get-DisplayLocked
vla-get-DisplayOLEScale
vla-get-DisplayScreenMenu
vla-get-DisplayScrollBars
vla-get-DisplaySilhouette
vla-get-DockedVisibleLines
vla-get-DockStatus
vla-get-Document
vla-get-Documents
vla-get-DogLegged
vla-get-DoglegLength
vla-get-Drafting
vla-get-DrawingDirection
vla-get-DrawLeaderOrderType
vla-get-DrawMLeaderOrderType
vla-get-DriversPath
vla-get-DWFFormat
vla-get-EdgeExtensionDistances
vla-get-EffectiveName
vla-get-Elevation
vla-get-ElevationModelSpace
vla-get-ElevationPaperSpace
vla-get-Enable
vla-get-EnableBlockRotation
vla-get-EnableBlockScale
vla-get-EnableDogleg
vla-get-EnableFrameText
vla-get-EnableLanding
vla-get-EnableStartupDialog
vla-get-EndAngle
vla-get-EndDraftAngle
vla-get-EndDraftMagnitude
vla-get-EndParameter
vla-get-EndPoint
vla-get-EndSmoothContinuity
vla-get-EndSmoothMagnitude
vla-get-EndSubMenuLevel
vla-get-EndTangent
vla-get-EnterpriseMenuFile
vla-get-EntityColor
vla-get-EntityTransparency
vla-get-Explodable
vla-get-ExtensionLineColor
vla-get-ExtensionLineExtend
vla-get-ExtensionLineOffset
vla-get-ExtensionLineWeight
vla-get-ExtLine1EndPoint
vla-get-ExtLine1Linetype
vla-get-ExtLine1Point
vla-get-ExtLine1StartPoint
vla-get-ExtLine1Suppress
vla-get-ExtLine2EndPoint
vla-get-ExtLine2Linetype
vla-get-ExtLine2Point
vla-get-ExtLine2StartPoint
vla-get-ExtLine2Suppress
vla-get-ExtLineFixedLen
vla-get-ExtLineFixedLenSuppress
vla-get-FaceCount
vla-get-Fade
vla-get-FieldLength
vla-get-File
vla-get-Files
vla-get-FirstSegmentAngleConstraint
vla-get-Fit
vla-get-FitPoints
vla-get-FitTolerance
vla-get-FloatingRows
vla-get-FlowDirection
vla-get-Flyout
vla-get-FontFile
vla-get-FontFileMap
vla-get-ForceLineInside
vla-get-ForegroundLinesColor
vla-get-ForegroundLinesEdgeTransparency
vla-get-ForegroundLinesFaceTransparency
vla-get-ForegroundLinesHiddenLine
vla-get-ForegroundLinesLayer
vla-get-ForegroundLinesLinetype
vla-get-ForegroundLinesLinetypeScale
vla-get-ForegroundLinesLineweight
vla-get-ForegroundLinesPlotStyleName
vla-get-ForegroundLinesVisible
vla-get-FractionFormat
vla-get-Freeze
vla-get-FullCRCValidation
vla-get-FullName
vla-get-FullScreenTrackingVector
vla-get-GenerationOptions
vla-get-GeoImageBrightness
vla-get-GeoImageContrast
vla-get-GeoImageFade
vla-get-GeoImageHeight
vla-get-GeoImagePosition
vla-get-GeoImageWidth
vla-get-Geolocate
vla-get-GradientAngle
vla-get-GradientCentered
vla-get-GradientColor1
vla-get-GradientColor2
vla-get-GradientName
vla-get-GraphicsWinLayoutBackgrndColor
vla-get-GraphicsWinModelBackgrndColor
vla-get-Green
vla-get-GridOn
vla-get-GripColorSelected
vla-get-GripColorUnselected
vla-get-GripSize
vla-get-Groups
vla-get-Handle
vla-get-HasAttributes
vla-get-HasExtensionDictionary
vla-get-HasLeader
vla-get-HasSheetView
vla-get-HasSubSelection
vla-get-HasVpAssociation
vla-get-HatchObjectType
vla-get-HatchStyle
vla-get-HeaderSuppressed
vla-get-Height
vla-get-HelpFilePath
vla-get-HelpString
vla-get-History
vla-get-HistoryLines
vla-get-HorizontalTextPosition
vla-get-HorzCellMargin
vla-get-HWND
vla-get-HyperlinkBase
vla-get-HyperlinkDisplayCursor
vla-get-Hyperlinks
vla-get-ImageFile
vla-get-ImageFrameHighlight
vla-get-ImageHeight
vla-get-ImageVisibility
vla-get-ImageWidth
vla-get-IncrementalSavePercent
vla-get-index
vla-get-IndicatorFillColor
vla-get-IndicatorTransparency
vla-get-InsertionPoint
vla-get-InsUnits
vla-get-InsUnitsFactor
vla-get-IntensityColorScheme
vla-get-IntersectionBoundaryColor
vla-get-IntersectionBoundaryDivisionLines
vla-get-IntersectionBoundaryLayer
vla-get-IntersectionBoundaryLinetype
vla-get-IntersectionBoundaryLinetypeScale
vla-get-IntersectionBoundaryLineweight
vla-get-IntersectionBoundaryPlotStyleName
vla-get-IntersectionBoundaryVisible
vla-get-IntersectionFillColor
vla-get-IntersectionFillFaceTransparency
vla-get-IntersectionFillHatchAngle
vla-get-IntersectionFillHatchPatternName
vla-get-IntersectionFillHatchPatternType
vla-get-IntersectionFillHatchScale
vla-get-IntersectionFillHatchSpacing
vla-get-IntersectionFillLayer
vla-get-IntersectionFillLinetype
vla-get-IntersectionFillLinetypeScale
vla-get-IntersectionFillLineweight
vla-get-IntersectionFillPlotStyleName
vla-get-IntersectionFillVisible
vla-get-Invisible
vla-get-IsCloned
vla-get-IsDynamicBlock
vla-get-IsLayout
vla-get-ISOPenWidth
vla-get-IsOwnerXlated
vla-get-IsPartial
vla-get-IsPeriodic
vla-get-IsPlanar
vla-get-IsPrimary
vla-get-IsQuiescent
vla-get-IsRational
vla-get-Issuer
vla-get-IsXRef
vla-get-ItemName
vla-get-JogAngle
vla-get-JogLocation
vla-get-Justification
vla-get-Key
vla-get-KeyboardAccelerator
vla-get-KeyboardPriority
vla-get-KeyLength
vla-get-Keywords
vla-get-KnotParameterization
vla-get-Knots
vla-get-Label
vla-get-LabelBlockId
vla-get-LandingGap
vla-get-LargeButtons
vla-get-LastHeight
vla-get-LastSavedBy
vla-get-Latitude
vla-get-Layer
vla-get-LayerOn
vla-get-LayerPropertyOverrides
vla-get-Layers
vla-get-LayerState
vla-get-Layout
vla-get-LayoutCreateViewport
vla-get-LayoutCrosshairColor
vla-get-LayoutDisplayMargins
vla-get-LayoutDisplayPaper
vla-get-LayoutDisplayPaperShadow
vla-get-LayoutId
vla-get-Layouts
vla-get-LayoutShowPlotSetup
vla-get-Leader1Point
vla-get-Leader2Point
vla-get-LeaderCount
vla-get-LeaderLineColor
vla-get-LeaderLineType
vla-get-LeaderLineTypeId
vla-get-LeaderLineWeight
vla-get-LeaderType
vla-get-Left
vla-get-Length
vla-get-LensLength
vla-get-Limits
vla-get-LinearScaleFactor
vla-get-LineSpacingDistance
vla-get-LineSpacingFactor
vla-get-LineSpacingStyle
vla-get-Linetype
vla-get-LinetypeGeneration
vla-get-Linetypes
vla-get-LinetypeScale
vla-get-LineWeight
vla-get-LineWeightDisplay
vla-get-LiveSectionEnabled
vla-get-LoadAcadLspInAllDocuments
vla-get-LocaleId
vla-get-Lock
vla-get-LockAspectRatio
vla-get-Locked
vla-get-LockPosition
vla-get-LogFileOn
vla-get-LogFilePath
vla-get-Longitude
vla-get-LowerLeftCorner
vla-get-Macro
vla-get-MainDictionary
vla-get-MaintainAssociativity
vla-get-MajorAxis
vla-get-MajorRadius
vla-get-Mask
vla-get-Material
vla-get-Materials
vla-get-MaxActiveViewports
vla-get-MaxAutoCADWindow
vla-get-MaxLeaderSegmentsPoints
vla-get-MClose
vla-get-MDensity
vla-get-Measurement
vla-get-MenuBar
vla-get-MenuFile
vla-get-MenuFileName
vla-get-MenuGroups
vla-get-Menus
vla-get-MinimumTableHeight
vla-get-MinimumTableWidth
vla-get-MinorAxis
vla-get-MinorRadius
vla-get-MLineScale
vla-get-Mode
vla-get-ModelCrosshairColor
vla-get-ModelSpace
vla-get-ModelType
vla-get-ModelView
vla-get-MomentOfInertia
vla-get-Monochrome
vla-get-MRUNumber
vla-get-MSpace
vla-get-MTextAttribute
vla-get-MTextAttributeContent
vla-get-MTextBoundaryWidth
vla-get-MTextDrawingDirection
vla-get-MVertexCount
vla-get-Name
vla-get-NameNoMnemonic
vla-get-NClose
vla-get-NDensity
vla-get-Normal
vla-get-Notes
vla-get-NumberOfControlPoints
vla-get-NumberOfCopies
vla-get-NumberOfFaces
vla-get-NumberOfFitPoints
vla-get-NumberOfLoops
vla-get-NumberOfVertices
vla-get-NumCellStyles
vla-get-NumCrossSections
vla-get-NumGuidePaths
vla-get-NumVertices
vla-get-NVertexCount
vla-get-ObjectID
vla-get-ObjectName
vla-get-ObjectSnapMode
vla-get-ObjectSortByPlotting
vla-get-ObjectSortByPSOutput
vla-get-ObjectSortByRedraws
vla-get-ObjectSortByRegens
vla-get-ObjectSortBySelection
vla-get-ObjectSortBySnap
vla-get-ObliqueAngle
vla-get-OleItemType
vla-get-OLELaunch
vla-get-OlePlotQuality
vla-get-OLEQuality
vla-get-OleSourceApp
vla-get-OnMenuBar
vla-get-OpenSave
vla-get-Origin
vla-get-OrthoOn
vla-get-Output
vla-get-OverrideCenter
vla-get-OverwritePropChanged
vla-get-OwnerID
vla-get-PageSetupOverridesTemplateFile
vla-get-PaperSpace
vla-get-PaperUnits
vla-get-Parent
vla-get-Password
vla-get-Path
vla-get-PatternAngle
vla-get-PatternDouble
vla-get-PatternName
vla-get-PatternScale
vla-get-PatternSpace
vla-get-PatternType
vla-get-Perimeter
vla-get-Periodic
vla-get-PickAdd
vla-get-PickAuto
vla-get-PickBoxSize
vla-get-PickDrag
vla-get-PickFirst
vla-get-PickfirstSelectionSet
vla-get-PickGroup
vla-get-Plot
vla-get-PlotConfigurations
vla-get-PlotHidden
vla-get-PlotLegacy
vla-get-PlotLogFilePath
vla-get-PlotOrigin
vla-get-PlotPolicy
vla-get-PlotRotation
vla-get-PlotStyleName
vla-get-Plottable
vla-get-PlotType
vla-get-PlotViewportBorders
vla-get-PlotViewportsFirst
vla-get-PlotWithLineweights
vla-get-PlotWithPlotStyles
vla-get-PolarTrackingVector
vla-get-Position
vla-get-PostScriptPrologFile
vla-get-Preferences
vla-get-Preset
vla-get-PrimaryUnitsPrecision
vla-get-PrincipalDirections
vla-get-PrincipalMoments
vla-get-PrinterConfigPath
vla-get-PrinterDescPath
vla-get-PrinterPaperSizeAlert
vla-get-PrinterSpoolAlert
vla-get-PrinterStyleSheetPath
vla-get-PrintFile
vla-get-PrintSpoolerPath
vla-get-PrintSpoolExecutable
vla-get-ProductOfInertia
vla-get-ProfileRotation
vla-get-Profiles
vla-get-PromptString
vla-get-PropertyName
vla-get-ProviderName
vla-get-ProviderType
vla-get-ProxyImage
vla-get-QNewTemplateFile
vla-get-QuietErrorMode
vla-get-RadiiOfGyration
vla-get-Radius
vla-get-RadiusRatio
vla-get-ReadOnly
vla-get-Red
vla-get-RegenerateTableSuppressed
vla-get-RegisteredApplications
vla-get-RenderSmoothness
vla-get-RepeatBottomLabels
vla-get-RepeatTopLabels
vla-get-RevisionNumber
vla-get-RevolutionAngle
vla-get-Rotation
vla-get-RoundDistance
vla-get-Rows
vla-get-RowSpacing
vla-get-SaveAsType
vla-get-Saved
vla-get-SavePreviewThumbnail
vla-get-scale
vla-get-ScaleFactor
vla-get-ScaleHeight
vla-get-ScaleLineweights
vla-get-ScaleWidth
vla-get-SCMCommandMode
vla-get-SCMDefaultMode
vla-get-SCMEditMode
vla-get-SCMTimeMode
vla-get-SCMTimeValue
vla-get-SecondPoint
vla-get-SecondSegmentAngleConstraint
vla-get-SectionManager
vla-get-SectionPlaneOffset
vla-get-Segmentation
vla-get-SegmentPerPolyline
vla-get-Selection
vla-get-SelectionSets
vla-get-SerialNumber
vla-get-Settings
vla-get-ShadePlot
vla-get-SheetView
vla-get-ShortcutMenu
vla-get-ShortCutMenuDisplay
vla-get-Show
vla-get-ShowAssociativity
vla-get-ShowClipped
vla-get-ShowCropped
vla-get-ShowHistory
vla-get-ShowIntensity
vla-get-ShowPlotStyles
vla-get-ShowProxyDialogBox
vla-get-ShowRasterImage
vla-get-ShowRotation
vla-get-ShowWarningMessages
vla-get-SingleDocumentMode
vla-get-SliceDepth
vla-get-Smoothness
vla-get-SnapBasePoint
vla-get-SnapOn
vla-get-SnapRotationAngle
vla-get-SolidFill
vla-get-SolidType
vla-get-SourceObjects
vla-get-SplineFrame
vla-get-SplineMethod
vla-get-StandardScale
vla-get-StandardScale2
vla-get-StartAngle
vla-get-StartDraftAngle
vla-get-StartDraftMagnitude
vla-get-StartParameter
vla-get-StartPoint
vla-get-StartSmoothContinuity
vla-get-StartSmoothMagnitude
vla-get-StartTangent
vla-get-State
vla-get-State2
vla-get-StatusId
vla-get-StoreSQLIndex
vla-get-StyleName
vla-get-StyleSheet
vla-get-Stylization
vla-get-Subject
vla-get-SubMenu
vla-get-SubUnitsFactor
vla-get-SubUnitsSuffix
vla-get-SummaryInfo
vla-get-SupportPath
vla-get-SuppressLeadingZeros
vla-get-SuppressTrailingZeros
vla-get-SuppressZeroFeet
vla-get-SuppressZeroInches
vla-get-SurfaceNormals
vla-get-SurfaceType
vla-get-SurfTrimAssociativity
vla-get-SymbolPosition
vla-get-System
vla-get-TableBreakFlowDirection
vla-get-TableBreakHeight
vla-get-TablesReadOnly
vla-get-TableStyleOverrides
vla-get-TabOrder
vla-get-TagString
vla-get-TaperAngle
vla-get-Target
vla-get-TempFileExtension
vla-get-TempFilePath
vla-get-TemplateDwgPath
vla-get-TemplateId
vla-get-TempXrefPath
vla-get-TextAlignmentPoint
vla-get-TextAlignmentType
vla-get-TextAngleType
vla-get-TextAttachmentDirection
vla-get-TextBackgroundFill
vla-get-TextBottomAttachmentType
vla-get-TextColor
vla-get-TextDirection
vla-get-TextEditor
vla-get-TextFill
vla-get-TextFillColor
vla-get-TextFont
vla-get-TextFontSize
vla-get-TextFontStyle
vla-get-TextFrameDisplay
vla-get-TextGap
vla-get-TextGenerationFlag
vla-get-TextHeight
vla-get-TextInside
vla-get-TextInsideAlign
vla-get-TextJustify
vla-get-TextLeftAttachmentType
vla-get-TextLineSpacingDistance
vla-get-TextLineSpacingFactor
vla-get-TextLineSpacingStyle
vla-get-TextMovement
vla-get-TextOutsideAlign
vla-get-TextOverride
vla-get-TextPosition
vla-get-TextPrecision
vla-get-TextPrefix
vla-get-TextRightAttachmentType
vla-get-TextRotation
vla-get-TextString
vla-get-TextStyle
vla-get-TextStyleName
vla-get-TextStyles
vla-get-TextSuffix
vla-get-TextTopAttachmentType
vla-get-TextureMapPath
vla-get-TextWidth
vla-get-TextWinBackgrndColor
vla-get-TextWinTextColor
vla-get-Thickness
vla-get-TimeServer
vla-get-Title
vla-get-TitleSuppressed
vla-get-ToleranceDisplay
vla-get-ToleranceHeightScale
vla-get-ToleranceJustification
vla-get-ToleranceLowerLimit
vla-get-TolerancePrecision
vla-get-ToleranceSuppressLeadingZeros
vla-get-ToleranceSuppressTrailingZeros
vla-get-ToleranceSuppressZeroFeet
vla-get-ToleranceSuppressZeroInches
vla-get-ToleranceUpperLimit
vla-get-Toolbars
vla-get-ToolPalettePath
vla-get-Top
vla-get-TopHeight
vla-get-TopRadius
vla-get-TotalAngle
vla-get-TotalLength
vla-get-TranslateIDs
vla-get-Transparency
vla-get-TrueColor
vla-get-TrueColorImages
vla-get-TurnHeight
vla-get-Turns
vla-get-TurnSlope
vla-get-Twist
vla-get-TwistAngle
vla-get-Type
vla-get-UCSIconAtOrigin
vla-get-UCSIconOn
vla-get-UCSPerViewport
vla-get-UIsolineDensity
vla-get-UnderlayLayerOverrideApplied
vla-get-UnderlayName
vla-get-UnderlayVisibility
vla-get-Unit
vla-get-UnitFactor
vla-get-Units
vla-get-UnitsFormat
vla-get-UnitsType
vla-get-UpperRightCorner
vla-get-UpsideDown
vla-get-URL
vla-get-URLDescription
vla-get-URLNamedLocation
vla-get-Used
vla-get-UseEntityColor
vla-get-UseLastPlotSettings
vla-get-User
vla-get-UserCoordinateSystems
vla-get-UseStandardScale
vla-get-Utility
vla-get-Value
vla-get-VBE
vla-get-Verify
vla-get-Version
vla-get-VertCellMargin
vla-get-VertexCount
vla-get-VerticalDirection
vla-get-VerticalTextPosition
vla-get-Vertices
vla-get-ViewingDirection
vla-get-ViewportDefault
vla-get-ViewportOn
vla-get-Viewports
vla-get-Views
vla-get-ViewToPlot
vla-get-VisibilityEdge1
vla-get-VisibilityEdge2
vla-get-VisibilityEdge3
vla-get-VisibilityEdge4
vla-get-Visible
vla-get-VIsolineDensity
vla-get-VisualStyle
vla-get-Volume
vla-get-Weights
vla-get-Width
vla-get-WindowLeft
vla-get-WindowState
vla-get-WindowTitle
vla-get-WindowTop
vla-get-WireframeType
vla-get-WorkspacePath
vla-get-XEffectiveScaleFactor
vla-get-XRefDatabase
vla-get-XrefDemandLoad
vla-get-XRefEdit
vla-get-XRefFadeIntensity
vla-get-XRefLayerVisibility
vla-get-XScaleFactor
vla-get-XVector
vla-get-YEffectiveScaleFactor
vla-get-YScaleFactor
vla-get-YVector
vla-get-ZEffectiveScaleFactor
vla-get-ZScaleFactor
vla-GetAcadState
vla-GetAlignment
vla-GetAlignment2
vla-GetAllProfileNames
vla-GetAngle
vla-GetAttachmentPoint
vla-GetAttributes
vla-GetAutoScale
vla-GetAutoScale2
vla-GetBackgroundColor
vla-GetBackgroundColor2
vla-GetBackgroundColorNone
vla-GetBitmaps
vla-GetBlockAttributeValue
vla-GetBlockAttributeValue2
vla-GetBlockRotation
vla-GetBlockScale
vla-GetBlockTableRecordId
vla-GetBlockTableRecordId2
vla-GetBoundingBox
vla-GetBreakHeight
vla-GetBulge
vla-GetCanonicalMediaNames
vla-GetCellAlignment
vla-GetCellBackgroundColor
vla-GetCellBackgroundColorNone
vla-GetCellClass
vla-GetCellContentColor
vla-GetCellDataType
vla-GetCellExtents
vla-GetCellFormat
vla-GetCellGridColor
vla-GetCellGridLineWeight
vla-GetCellGridVisibility
vla-GetCellState
vla-GetCellStyle
vla-GetCellStyleOverrides
vla-GetCellStyles
vla-GetCellTextHeight
vla-GetCellTextStyle
vla-GetCellType
vla-GetCellValue
vla-GetColor
vla-GetColor2
vla-GetColumnName
vla-GetColumnWidth
vla-GetConstantAttributes
vla-GetContentColor
vla-GetContentColor2
vla-GetContentLayout
vla-GetContentType
vla-GetControlPoint
vla-GetCorner
vla-GetCustomByIndex
vla-GetCustomByKey
vla-GetCustomData
vla-GetCustomScale
vla-GetDataFormat
vla-GetDataType
vla-GetDataType2
vla-GetDistance
vla-GetDoglegDirection
vla-GetDynamicBlockProperties
vla-GetEntity
vla-GetExtensionDictionary
vla-GetFieldId
vla-GetFieldId2
vla-GetFitPoint
vla-GetFont
vla-GetFormat
vla-GetFormat2
vla-GetFormula
vla-GetFullDrawOrder
vla-GetGridColor
vla-GetGridColor2
vla-GetGridDoubleLineSpacing
vla-GetGridLineStyle
vla-GetGridLinetype
vla-GetGridLineWeight
vla-GetGridLineWeight2
vla-GetGridSpacing
vla-GetGridVisibility
vla-GetGridVisibility2
vla-GetHasFormula
vla-GetInput
vla-GetInteger
vla-GetInterfaceObject
vla-GetInvisibleEdge
vla-GetIsCellStyleInUse
vla-GetIsMergeAllEnabled
vla-GetKeyword
vla-GetLeaderIndex
vla-GetLeaderLineIndexes
vla-GetLeaderLineVertices
vla-GetLiveSection
vla-GetLocaleMediaName
vla-GetLoopAt
vla-GetMargin
vla-GetMinimumColumnWidth
vla-GetMinimumRowHeight
vla-GetName
vla-GetObject
vla-GetObjectIdString
vla-GetOrientation
vla-GetOverride
vla-GetPaperMargins
vla-GetPaperSize
vla-GetPlotDeviceNames
vla-GetPlotStyleTableNames
vla-GetPoint
vla-GetProjectFilePath
vla-GetReal
vla-GetRelativeDrawOrder
vla-GetRemoteFile
vla-GetRotation
vla-GetRowHeight
vla-GetRowType
vla-GetScale
vla-GetSectionTypeSettings
vla-GetSnapSpacing
vla-GetString
vla-GetSubEntity
vla-GetSubSelection
vla-GetText
vla-GetTextHeight
vla-GetTextHeight2
vla-GetTextRotation
vla-GetTextString
vla-GetTextStyle
vla-GetTextStyle2
vla-GetTextStyleId
vla-GetUCSMatrix
vla-GetUniqueCellStyleName
vla-GetUniqueSectionName
vla-GetValue
vla-GetVariable
vla-GetVertexCount
vla-GetWeight
vla-GetWidth
vla-GetWindowToPlot
vla-GetXData
vla-GetXRecordData

vla-HandleToObject
vla-Highlight
vla-HitTest

vla-Import
vla-ImportProfile
vla-InitializeUserInput
vla-InsertBlock
vla-InsertColumns
vla-InsertColumnsAndInherit
vla-InsertInMenuBar
vla-InsertLoopAt
vla-InsertMenuInMenuBar
vla-InsertRows
vla-InsertRowsAndInherit
vla-IntersectWith
vla-IsContentEditable
vla-IsEmpty
vla-IsFormatEditable
vla-IsMergeAllEnabled
vla-IsMergedCell
vla-IsRemoteFile
vla-IsURL
vla-Item

vla-LaunchBrowserDialog
vla-ListArx
vla-Load
vla-LoadArx
vla-LoadDVB
vla-LoadShapeFile

vla-MergeCells
vla-Mirror
vla-Mirror3D
vla-Modified
vla-Move
vla-MoveAbove
vla-MoveBelow
vla-MoveContent
vla-MoveToBottom
vla-MoveToTop

vla-New
vla-NumCustomInfo

C:VLA-OBJ
C:VLA-OBJ-DUMP
vla-ObjectIDToObject
vla-Offset
vla-OnModified
vla-Open
vlax-ename->vla-object
vlax-vla-object->ename

vla-PlotToDevice
vla-PlotToFile
vla-PolarPoint
vla-PostCommand
vla-Prompt
vla-PurgeAll
vla-PurgeFitData
vla-put-Action
vla-put-ActiveDimStyle
vla-put-ActiveDocument
vla-put-ActiveInvProject
vla-put-ActiveLayer
vla-put-ActiveLayout
vla-put-ActiveLinetype
vla-put-ActiveMaterial
vla-put-ActiveProfile
vla-put-ActivePViewport
vla-put-ActiveSpace
vla-put-ActiveTextStyle
vla-put-ActiveUCS
vla-put-ActiveViewport
vla-put-ADCInsertUnitsDefaultSource
vla-put-ADCInsertUnitsDefaultTarget
vla-put-AdjustForBackground
vla-put-Algorithm
vla-put-Alignment
vla-put-AlignmentPointAcquisition
vla-put-AlignSpace
vla-put-AllowLongSymbolNames
vla-put-AllowManualHeights
vla-put-AllowManualPositions
vla-put-AltFontFile
vla-put-Altitude
vla-put-AltRoundDistance
vla-put-AltSubUnitsFactor
vla-put-AltSubUnitsSuffix
vla-put-AltSuppressLeadingZeros
vla-put-AltSuppressTrailingZeros
vla-put-AltSuppressZeroFeet
vla-put-AltSuppressZeroInches
vla-put-AltTabletMenuFile
vla-put-AltTextPrefix
vla-put-AltTextSuffix
vla-put-AltTolerancePrecision
vla-put-AltToleranceSuppressLeadingZeros
vla-put-AltToleranceSuppressTrailingZeros
vla-put-AltToleranceSuppressZeroFeet
vla-put-AltToleranceSuppressZeroInches
vla-put-AltUnits
vla-put-AltUnitsFormat
vla-put-AltUnitsPrecision
vla-put-AltUnitsScale
vla-put-AngleFormat
vla-put-AngleVertex
vla-put-Annotation
vla-put-Annotative
vla-put-ArcEndParam
vla-put-ArcPoint
vla-put-ArcSmoothness
vla-put-ArcStartParam
vla-put-Area
vla-put-Arrowhead1Block
vla-put-Arrowhead1Type
vla-put-Arrowhead2Block
vla-put-Arrowhead2Type
vla-put-ArrowheadBlock
vla-put-ArrowheadSize
vla-put-ArrowheadType
vla-put-ArrowSize
vla-put-ArrowSymbol
vla-put-AssociativeHatch
vla-put-AttachmentPoint
vla-put-Author
vla-put-AutoAudit
vla-put-AutomaticPlotLog
vla-put-AutoSaveInterval
vla-put-AutoSavePath
vla-put-AutoSnapAperture
vla-put-AutoSnapApertureSize
vla-put-AutoSnapMagnet
vla-put-AutoSnapMarker
vla-put-AutoSnapMarkerColor
vla-put-AutoSnapMarkerSize
vla-put-AutoSnapToolTip
vla-put-AutoTrackingVecColor
vla-put-AutoTrackTooltip
vla-put-AxisPosition
vla-put-BackgroundColor
vla-put-BackgroundFill
vla-put-BackgroundLinesColor
vla-put-BackgroundLinesHiddenLine
vla-put-BackgroundLinesLayer
vla-put-BackgroundLinesLinetype
vla-put-BackgroundLinesLinetypeScale
vla-put-BackgroundLinesLineweight
vla-put-BackgroundLinesPlotStyleName
vla-put-BackgroundLinesVisible
vla-put-Backward
vla-put-Bank
vla-put-BasePoint
vla-put-BaseRadius
vla-put-BatchPlotProgress
vla-put-BeepOnError
vla-put-BigFontFile
vla-put-BitFlags
vla-put-Block
vla-put-BlockColor
vla-put-BlockConnectionType
vla-put-BlockRotation
vla-put-BlockScale
vla-put-BlockScaling
vla-put-BottomHeight
vla-put-BreaksEnabled
vla-put-BreakSize
vla-put-BreakSpacing
vla-put-Brightness
vla-put-CanonicalMediaName
vla-put-CategoryName
vla-put-Center
vla-put-CenterMarkSize
vla-put-CenterPlot
vla-put-CenterPoint
vla-put-CenterType
vla-put-Check
vla-put-ChordPoint
vla-put-Circumference
vla-put-ClippingEnabled
vla-put-Closed
vla-put-Closed2
vla-put-Color
vla-put-ColorBookPath
vla-put-ColorIndex
vla-put-ColorMethod
vla-put-ColorScheme
vla-put-Columns
vla-put-ColumnSpacing
vla-put-ColumnWidth
vla-put-CommandDisplayName
vla-put-Comment
vla-put-Comments
vla-put-ConfigName
vla-put-Constant
vla-put-ConstantWidth
vla-put-Constrain
vla-put-ContentBlockName
vla-put-ContentBlockType
vla-put-ContentType
vla-put-ContinuousPlotLog
vla-put-ContourLinesPerSurface
vla-put-Contrast
vla-put-ControlPoints
vla-put-Coordinate
vla-put-Coordinates
vla-put-CreaseLevel
vla-put-CreaseType
vla-put-CreateBackup
vla-put-CurrentSectionType
vla-put-CursorSize
vla-put-CurveTangencyLinesColor
vla-put-CurveTangencyLinesLayer
vla-put-CurveTangencyLinesLinetype
vla-put-CurveTangencyLinesLinetypeScale
vla-put-CurveTangencyLinesLineweight
vla-put-CurveTangencyLinesPlotStyleName
vla-put-CurveTangencyLinesVisible
vla-put-CustomDictionary
vla-put-CustomIconPath
vla-put-CustomScale
vla-put-CvHullDisplay
vla-put-DecimalSeparator
vla-put-DefaultInternetURL
vla-put-DefaultOutputDevice
vla-put-DefaultPlotStyleForLayer
vla-put-DefaultPlotStyleForObjects
vla-put-DefaultPlotStyleTable
vla-put-DefaultPlotToFilePath
vla-put-Degree2
vla-put-DemandLoadARXApp
vla-put-Description
vla-put-DestinationBlock
vla-put-DestinationFile
vla-put-Diameter
vla-put-DimConstrDesc
vla-put-DimConstrExpression
vla-put-DimConstrForm
vla-put-DimConstrName
vla-put-DimConstrReference
vla-put-DimConstrValue
vla-put-DimensionLineColor
vla-put-DimensionLineExtend
vla-put-DimensionLinetype
vla-put-DimensionLineWeight
vla-put-DimLine1Suppress
vla-put-DimLine2Suppress
vla-put-DimLineInside
vla-put-DimLineSuppress
vla-put-DimTxtDirection
vla-put-Direction
vla-put-DirectionVector
vla-put-DisplayGrips
vla-put-DisplayGripsWithinBlocks
vla-put-DisplayLayoutTabs
vla-put-DisplayLocked
vla-put-DisplayOLEScale
vla-put-DisplayScreenMenu
vla-put-DisplayScrollBars
vla-put-DisplaySilhouette
vla-put-DockedVisibleLines
vla-put-DogLegged
vla-put-DoglegLength
vla-put-DrawingDirection
vla-put-DrawLeaderOrderType
vla-put-DrawMLeaderOrderType
vla-put-DriversPath
vla-put-DWFFormat
vla-put-EdgeExtensionDistances
vla-put-Elevation
vla-put-ElevationModelSpace
vla-put-ElevationPaperSpace
vla-put-Enable
vla-put-EnableBlockRotation
vla-put-EnableBlockScale
vla-put-EnableBreak
vla-put-EnableDogleg
vla-put-EnableFrameText
vla-put-EnableLanding
vla-put-EnableStartupDialog
vla-put-EndAngle
vla-put-EndDraftAngle
vla-put-EndDraftMagnitude
vla-put-EndParameter
vla-put-EndPoint
vla-put-EndSmoothContinuity
vla-put-EndSmoothMagnitude
vla-put-EndSubMenuLevel
vla-put-EndTangent
vla-put-EnterpriseMenuFile
vla-put-EntityColor
vla-put-EntityTransparency
vla-put-Explodable
vla-put-ExtensionLineColor
vla-put-ExtensionLineExtend
vla-put-ExtensionLineOffset
vla-put-ExtensionLineWeight
vla-put-ExtLine1EndPoint
vla-put-ExtLine1Linetype
vla-put-ExtLine1Point
vla-put-ExtLine1StartPoint
vla-put-ExtLine1Suppress
vla-put-ExtLine2EndPoint
vla-put-ExtLine2Linetype
vla-put-ExtLine2Point
vla-put-ExtLine2StartPoint
vla-put-ExtLine2Suppress
vla-put-ExtLineFixedLen
vla-put-ExtLineFixedLenSuppress
vla-put-Fade
vla-put-FieldLength
vla-put-File
vla-put-FirstSegmentAngleConstraint
vla-put-Fit
vla-put-FitPoints
vla-put-FitTolerance
vla-put-FloatingRows
vla-put-FlowDirection
vla-put-FontFile
vla-put-FontFileMap
vla-put-ForceLineInside
vla-put-ForegroundLinesColor
vla-put-ForegroundLinesEdgeTransparency
vla-put-ForegroundLinesFaceTransparency
vla-put-ForegroundLinesHiddenLine
vla-put-ForegroundLinesLayer
vla-put-ForegroundLinesLinetype
vla-put-ForegroundLinesLinetypeScale
vla-put-ForegroundLinesLineweight
vla-put-ForegroundLinesPlotStyleName
vla-put-ForegroundLinesVisible
vla-put-FractionFormat
vla-put-Freeze
vla-put-FullCRCValidation
vla-put-FullScreenTrackingVector
vla-put-GenerationOptions
vla-put-GeoImageBrightness
vla-put-GeoImageContrast
vla-put-GeoImageFade
vla-put-Geolocate
vla-put-GradientAngle
vla-put-GradientCentered
vla-put-GradientColor1
vla-put-GradientColor2
vla-put-GradientName
vla-put-GraphicsWinLayoutBackgrndColor
vla-put-GraphicsWinModelBackgrndColor
vla-put-GridOn
vla-put-GripColorSelected
vla-put-GripColorUnselected
vla-put-GripSize
vla-put-HasLeader
vla-put-HasVpAssociation
vla-put-HatchObjectType
vla-put-HatchStyle
vla-put-HeaderSuppressed
vla-put-Height
vla-put-HelpFilePath
vla-put-HelpString
vla-put-History
vla-put-HistoryLines
vla-put-HorizontalTextPosition
vla-put-HorzCellMargin
vla-put-HyperlinkBase
vla-put-HyperlinkDisplayCursor
vla-put-ImageFile
vla-put-ImageFrameHighlight
vla-put-ImageHeight
vla-put-ImageVisibility
vla-put-ImageWidth
vla-put-IncrementalSavePercent
vla-put-IndicatorFillColor
vla-put-IndicatorTransparency
vla-put-InsertionPoint
vla-put-IntensityColorScheme
vla-put-IntersectionBoundaryColor
vla-put-IntersectionBoundaryDivisionLines
vla-put-IntersectionBoundaryLayer
vla-put-IntersectionBoundaryLinetype
vla-put-IntersectionBoundaryLinetypeScale
vla-put-IntersectionBoundaryLineweight
vla-put-IntersectionBoundaryPlotStyleName
vla-put-IntersectionBoundaryVisible
vla-put-IntersectionFillColor
vla-put-IntersectionFillFaceTransparency
vla-put-IntersectionFillHatchAngle
vla-put-IntersectionFillHatchPatternName
vla-put-IntersectionFillHatchPatternType
vla-put-IntersectionFillHatchScale
vla-put-IntersectionFillHatchSpacing
vla-put-IntersectionFillLayer
vla-put-IntersectionFillLinetype
vla-put-IntersectionFillLinetypeScale
vla-put-IntersectionFillLineweight
vla-put-IntersectionFillPlotStyleName
vla-put-IntersectionFillVisible
vla-put-Invisible
vla-put-ISOPenWidth
vla-put-IsPartial
vla-put-Issuer
vla-put-ItemName
vla-put-JogAngle
vla-put-JogLocation
vla-put-Justification
vla-put-KeyboardAccelerator
vla-put-KeyboardPriority
vla-put-KeyLength
vla-put-Keywords
vla-put-KnotParameterization
vla-put-Knots
vla-put-Label
vla-put-LabelBlockId
vla-put-LandingGap
vla-put-LargeButtons
vla-put-LastHeight
vla-put-LastSavedBy
vla-put-Latitude
vla-put-Layer
vla-put-LayerOn
vla-put-LayerState
vla-put-LayoutCreateViewport
vla-put-LayoutCrosshairColor
vla-put-LayoutDisplayMargins
vla-put-LayoutDisplayPaper
vla-put-LayoutDisplayPaperShadow
vla-put-LayoutId
vla-put-LayoutShowPlotSetup
vla-put-Leader1Point
vla-put-Leader2Point
vla-put-LeaderLength
vla-put-LeaderLineColor
vla-put-LeaderLineType
vla-put-LeaderLineTypeId
vla-put-LeaderLineWeight
vla-put-LeaderType
vla-put-Left
vla-put-Length
vla-put-LensLength
vla-put-Limits
vla-put-LinearScaleFactor
vla-put-LineSpacingDistance
vla-put-LineSpacingFactor
vla-put-LineSpacingStyle
vla-put-Linetype
vla-put-LinetypeGeneration
vla-put-LinetypeScale
vla-put-LineWeight
vla-put-LineWeightDisplay
vla-put-LiveSectionEnabled
vla-put-LoadAcadLspInAllDocuments
vla-put-Lock
vla-put-LockAspectRatio
vla-put-Locked
vla-put-LockPosition
vla-put-LogFileOn
vla-put-LogFilePath
vla-put-Longitude
vla-put-Macro
vla-put-MainDictionary
vla-put-MaintainAssociativity
vla-put-MajorAxis
vla-put-MajorRadius
vla-put-Mask
vla-put-Material
vla-put-MaxActiveViewports
vla-put-MaxAutoCADWindow
vla-put-MaxLeaderSegmentsPoints
vla-put-MClose
vla-put-MDensity
vla-put-MenuFile
vla-put-MinorRadius
vla-put-MLineScale
vla-put-Mode
vla-put-ModelCrosshairColor
vla-put-ModelView
vla-put-Monochrome
vla-put-MSpace
vla-put-MTextAttribute
vla-put-MTextAttributeContent
vla-put-MTextBoundaryWidth
vla-put-MTextDrawingDirection
vla-put-Name
vla-put-NClose
vla-put-NDensity
vla-put-Normal
vla-put-Notes
vla-put-NumberOfCopies
vla-put-ObjectSnapMode
vla-put-ObjectSortByPlotting
vla-put-ObjectSortByPSOutput
vla-put-ObjectSortByRedraws
vla-put-ObjectSortByRegens
vla-put-ObjectSortBySelection
vla-put-ObjectSortBySnap
vla-put-ObliqueAngle
vla-put-OleItemType
vla-put-OLELaunch
vla-put-OlePlotQuality
vla-put-OLEQuality
vla-put-OleSourceApp
vla-put-Origin
vla-put-OrthoOn
vla-put-OverrideCenter
vla-put-PageSetupOverridesTemplateFile
vla-put-PaperUnits
vla-put-Password
vla-put-Path
vla-put-PatternAngle
vla-put-PatternDouble
vla-put-PatternScale
vla-put-PatternSpace
vla-put-Periodic
vla-put-PickAdd
vla-put-PickAuto
vla-put-PickBoxSize
vla-put-PickDrag
vla-put-PickFirst
vla-put-PickGroup
vla-put-PlotHidden
vla-put-PlotLegacy
vla-put-PlotLogFilePath
vla-put-PlotOrigin
vla-put-PlotPolicy
vla-put-PlotRotation
vla-put-PlotStyleName
vla-put-Plottable
vla-put-PlotType
vla-put-PlotViewportBorders
vla-put-PlotViewportsFirst
vla-put-PlotWithLineweights
vla-put-PlotWithPlotStyles
vla-put-PolarTrackingVector
vla-put-Position
vla-put-PostScriptPrologFile
vla-put-Preset
vla-put-PrimaryUnitsPrecision
vla-put-PrinterConfigPath
vla-put-PrinterDescPath
vla-put-PrinterPaperSizeAlert
vla-put-PrinterSpoolAlert
vla-put-PrinterStyleSheetPath
vla-put-PrintFile
vla-put-PrintSpoolerPath
vla-put-PrintSpoolExecutable
vla-put-ProfileRotation
vla-put-PromptString
vla-put-ProviderName
vla-put-ProviderType
vla-put-ProxyImage
vla-put-QNewTemplateFile
vla-put-QuietErrorMode
vla-put-Radius
vla-put-RadiusRatio
vla-put-RegenerateTableSuppressed
vla-put-RenderSmoothness
vla-put-RepeatBottomLabels
vla-put-RepeatTopLabels
vla-put-RevisionNumber
vla-put-RevolutionAngle
vla-put-Rotation
vla-put-RoundDistance
vla-put-RowHeight
vla-put-Rows
vla-put-RowSpacing
vla-put-SaveAsType
vla-put-SavePreviewThumbnail
vla-put-scale
vla-put-ScaleFactor
vla-put-ScaleHeight
vla-put-ScaleLineweights
vla-put-ScaleWidth
vla-put-SCMCommandMode
vla-put-SCMDefaultMode
vla-put-SCMEditMode
vla-put-SCMTimeMode
vla-put-SCMTimeValue
vla-put-SecondPoint
vla-put-SecondSegmentAngleConstraint
vla-put-SectionPlaneOffset
vla-put-SegmentPerPolyline
vla-put-SerialNumber
vla-put-ShadePlot
vla-put-SheetView
vla-put-ShortCutMenuDisplay
vla-put-ShowAssociativity
vla-put-ShowClipped
vla-put-ShowCropped
vla-put-ShowHistory
vla-put-ShowIntensity
vla-put-ShowPlotStyles
vla-put-ShowProxyDialogBox
vla-put-ShowRasterImage
vla-put-ShowRotation
vla-put-ShowWarningMessages
vla-put-SingleDocumentMode
vla-put-SliceDepth
vla-put-Smoothness
vla-put-SnapBasePoint
vla-put-SnapOn
vla-put-SnapRotationAngle
vla-put-SolidFill
vla-put-SourceObjects
vla-put-SplineFrame
vla-put-SplineMethod
vla-put-StandardScale
vla-put-StandardScale2
vla-put-StartAngle
vla-put-StartDraftAngle
vla-put-StartDraftMagnitude
vla-put-StartParameter
vla-put-StartPoint
vla-put-StartSmoothContinuity
vla-put-StartSmoothMagnitude
vla-put-StartTangent
vla-put-State
vla-put-State2
vla-put-StoreSQLIndex
vla-put-StyleName
vla-put-StyleSheet
vla-put-Stylization
vla-put-Subject
vla-put-SubUnitsFactor
vla-put-SubUnitsSuffix
vla-put-SupportPath
vla-put-SuppressLeadingZeros
vla-put-SuppressTrailingZeros
vla-put-SuppressZeroFeet
vla-put-SuppressZeroInches
vla-put-SurfaceNormals
vla-put-SurfTrimAssociativity
vla-put-SymbolPosition
vla-put-TableBreakFlowDirection
vla-put-TableBreakHeight
vla-put-TablesReadOnly
vla-put-TabOrder
vla-put-TagString
vla-put-TaperAngle
vla-put-Target
vla-put-TempFileExtension
vla-put-TempFilePath
vla-put-TemplateDwgPath
vla-put-TemplateId
vla-put-TempXrefPath
vla-put-TextAlignmentPoint
vla-put-TextAlignmentType
vla-put-TextAngleType
vla-put-TextAttachmentDirection
vla-put-TextBackgroundFill
vla-put-TextBottomAttachmentType
vla-put-TextColor
vla-put-TextDirection
vla-put-TextEditor
vla-put-TextFill
vla-put-TextFillColor
vla-put-TextFont
vla-put-TextFontSize
vla-put-TextFontStyle
vla-put-TextFrameDisplay
vla-put-TextGap
vla-put-TextGenerationFlag
vla-put-TextHeight
vla-put-TextInside
vla-put-TextInsideAlign
vla-put-TextJustify
vla-put-TextLeftAttachmentType
vla-put-TextLineSpacingDistance
vla-put-TextLineSpacingFactor
vla-put-TextLineSpacingStyle
vla-put-TextMovement
vla-put-TextOutsideAlign
vla-put-TextOverride
vla-put-TextPosition
vla-put-TextPrecision
vla-put-TextPrefix
vla-put-TextRightAttachmentType
vla-put-TextRotation
vla-put-TextString
vla-put-TextStyle
vla-put-TextStyleName
vla-put-TextSuffix
vla-put-TextTopAttachmentType
vla-put-TextureMapPath
vla-put-TextWidth
vla-put-TextWinBackgrndColor
vla-put-TextWinTextColor
vla-put-Thickness
vla-put-TimeServer
vla-put-Title
vla-put-TitleSuppressed
vla-put-ToleranceDisplay
vla-put-ToleranceHeightScale
vla-put-ToleranceJustification
vla-put-ToleranceLowerLimit
vla-put-TolerancePrecision
vla-put-ToleranceSuppressLeadingZeros
vla-put-ToleranceSuppressTrailingZeros
vla-put-ToleranceSuppressZeroFeet
vla-put-ToleranceSuppressZeroInches
vla-put-ToleranceUpperLimit
vla-put-ToolPalettePath
vla-put-Top
vla-put-TopHeight
vla-put-TopRadius
vla-put-TranslateIDs
vla-put-Transparency
vla-put-TrueColor
vla-put-TrueColorImages
vla-put-TurnHeight
vla-put-Turns
vla-put-Twist
vla-put-TwistAngle
vla-put-Type
vla-put-UCSIconAtOrigin
vla-put-UCSIconOn
vla-put-UCSPerViewport
vla-put-UIsolineDensity
vla-put-UnderlayLayerOverrideApplied
vla-put-UnderlayName
vla-put-UnderlayVisibility
vla-put-Units
vla-put-UnitsFormat
vla-put-UpsideDown
vla-put-URL
vla-put-URLDescription
vla-put-URLNamedLocation
vla-put-UseEntityColor
vla-put-UseLastPlotSettings
vla-put-UseStandardScale
vla-put-Value
vla-put-Verify
vla-put-VertCellMargin
vla-put-VerticalDirection
vla-put-VerticalTextPosition
vla-put-Vertices
vla-put-ViewingDirection
vla-put-ViewportDefault
vla-put-ViewportOn
vla-put-ViewToPlot
vla-put-VisibilityEdge1
vla-put-VisibilityEdge2
vla-put-VisibilityEdge3
vla-put-VisibilityEdge4
vla-put-Visible
vla-put-VIsolineDensity
vla-put-VisualStyle
vla-put-Weights
vla-put-Width
vla-put-WindowLeft
vla-put-WindowState
vla-put-WindowTop
vla-put-WireframeType
vla-put-WorkspacePath
vla-put-XEffectiveScaleFactor
vla-put-XrefDemandLoad
vla-put-XRefEdit
vla-put-XRefFadeIntensity
vla-put-XRefLayerVisibility
vla-put-XScaleFactor
vla-put-XVector
vla-put-YEffectiveScaleFactor
vla-put-YScaleFactor
vla-put-YVector
vla-put-ZEffectiveScaleFactor
vla-put-ZScaleFactor
vla-PutRemoteFile

vla-Quit

vla-RealToString
vla-RecomputeTableBlock
vla-RefreshPlotDeviceInfo
vla-Regen
vla-Reload
vla-Remove
vla-RemoveAllOverrides
vla-RemoveCustomByIndex
vla-RemoveCustomByKey
vla-RemoveFromMenuBar
vla-RemoveItems
vla-RemoveLeader
vla-RemoveLeaderLine
vla-RemoveMenuFromMenuBar
vla-RemoveVertex
vla-Rename
vla-RenameCellStyle
vla-RenameProfile
vla-Replace
vla-ReselectSubRegion
vla-ResetBlock
vla-ResetCellValue
vla-ResetProfile
vla-Restore
vla-Reverse
vla-Rotate
vla-Rotate3D
vla-RunMacro

vla-Save
vla-SaveAs
vla-ScaleEntity
vla-SectionSolid
vla-Select
vla-SelectAtPoint
vla-SelectByPolygon
vla-SelectOnScreen
vla-SelectSubRegion
vla-SendCommand
vla-SendModelessOperationEnded
vla-SendModelessOperationStart
vla-SetAlignment
vla-SetAlignment2
vla-SetAutoScale
vla-SetAutoScale2
vla-SetBackgroundColor
vla-SetBackgroundColor2
vla-SetBackgroundColorNone
vla-SetBitmaps
vla-SetBlockAttributeValue
vla-SetBlockAttributeValue2
vla-SetBlockRotation
vla-SetBlockScale
vla-SetBlockTableRecordId
vla-SetBlockTableRecordId2
vla-SetBreakHeight
vla-SetBulge
vla-SetCellAlignment
vla-SetCellBackgroundColor
vla-SetCellBackgroundColorNone
vla-SetCellClass
vla-SetCellContentColor
vla-SetCellDataType
vla-SetCellFormat
vla-SetCellGridColor
vla-SetCellGridLineWeight
vla-SetCellGridVisibility
vla-SetCellState
vla-SetCellStyle
vla-SetCellTextHeight
vla-SetCellTextStyle
vla-SetCellType
vla-SetCellValue
vla-SetCellValueFromText
vla-SetColor
vla-SetColor2
vla-SetColorBookColor
vla-SetColumnName
vla-SetColumnWidth
vla-SetContentColor
vla-SetContentColor2
vla-SetContentLayout
vla-SetControlPoint
vla-SetCustomByIndex
vla-SetCustomByKey
vla-SetCustomData
vla-SetCustomScale
vla-SetDatabase
vla-SetDataFormat
vla-SetDataType
vla-SetDataType2
vla-SetDoglegDirection
vla-SetFieldId
vla-SetFieldId2
vla-SetFitPoint
vla-SetFont
vla-SetFormat
vla-SetFormat2
vla-SetFormula
vla-SetGridColor
vla-SetGridColor2
vla-SetGridDoubleLineSpacing
vla-SetGridLineStyle
vla-SetGridLinetype
vla-SetGridLineWeight
vla-SetGridLineWeight2
vla-SetGridSpacing
vla-SetGridVisibility
vla-SetGridVisibility2
vla-SetInvisibleEdge
vla-SetLayoutsToPlot
vla-SetLeaderLineVertices
vla-SetMargin
vla-SetNames
vla-SetOverride
vla-SetPattern
vla-SetProjectFilePath
vla-SetRelativeDrawOrder
vla-SetRGB
vla-SetRotation
vla-SetRowHeight
vla-SetScale
vla-SetSnapSpacing
vla-SetSubSelection
vla-SetTemplateId
vla-SetText
vla-SetTextHeight
vla-SetTextHeight2
vla-SetTextRotation
vla-SetTextString
vla-SetTextStyle
vla-SetTextStyle2
vla-SetTextStyleId
vla-SetToolTip
vla-SetValue
vla-SetValueFromText
vla-SetVariable
vla-SetView
vla-SetWeight
vla-SetWidth
vla-SetWindowToPlot
vla-SetXData
vla-SetXRecordData
vla-SliceSolid
vla-Split
vla-StartBatchMode
vla-StartUndoMark
vla-SwapOrder
vla-SyncModelView

vla-TransformBy
vla-TranslateCoordinates

vla-Unload
vla-UnloadArx
vla-UnloadDVB
vla-UnmergeCells
vla-Update
vla-UpdateMTextAttribute

vla-Wblock

vla-ZoomAll
vla-ZoomCenter
vla-ZoomExtents
vla-ZoomPickWindow
vla-ZoomPrevious
vla-ZoomScaled
vla-ZoomWindow

