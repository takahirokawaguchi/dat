;(load "~/program/skill/dat/test/dat-test.il")
(load "~/program/skill/dat/source/dat-front.il")

;;; デバッグのためのユーティリティ関数
(defun scv ()
       (geGetWindowCellView ))
(defun gs ()
       (car (geGetSelSet scv)))
(defun l ()
       (load "~/program/skill/dat/dat-rewrite.il")
       (displayArrivalTime))
(defun zi (x y)
       (hiZoomIn (hiGetCurrentWindow) (ruGCtoSBB (list x y))))

(defun deleteAllPropsOfInstTerms (scv)
       (foreach x scv~>instances
                (foreach y x~>instTerms
                         (deleteAllProp y))))

(defun deleteAllProp (i)
       (foreach x i~>prop~>name
                (dbDeletePropByName i x)))

(defun testDat (scv outport)
       (displayArrivalTime scv)
       (fprintf out "==========%L==========\n" scv~>cellName)
       (fprintf out "%L\n\n%L\n\n"
                (mapcar 'outputTerms
                             (setof x scv~>instances (instIsConnectedOpin x)))~>arvTimeMin
                 (mapcar 'outputTerms
                             (setof x scv~>instances (instIsConnectedOpin x)))~>arvTimeMax
                 )
       )

(defun testDatAllLargeScaleAdp ()
       (setq out (outfile "testDatAllLargeScaleAdp.txt"))
       (setq scv (dbOpenCellViewByType "largescale_adp" "FPA_16bit" "schematic"))
       (fprintf out "time is %L\n\n\n\n" (measureTime (testDat scv out)))
       (setq scv (dbOpenCellViewByType "largescale_adp" "FPA_32bit" "schematic"))
       (fprintf out "time is %L\n\n\n\n" (measureTime (testDat scv out)))
       (setq scv (dbOpenCellViewByType "largescale_adp" "RDP4x4_ver10" "schematic"))
       (fprintf out "time is %L\n\n\n\n" (measureTime (testDat scv out)))
       (setq scv (dbOpenCellViewByType "largescale_adp" "FPM_16bit" "schematic"))
       (fprintf out "time is %L\n\n\n\n" (measureTime (testDat scv out)))
       ;(setq scv (dbOpenCellViewByType "largescale_stp" "CORE1a_ver5" "schematic"))
       ;(testDat scv out)
       (close out)
       )
;(defun appendList (xs)
;       (if (cdr xs)
;           then
;           (append (car xs) (appendList (cdr xs)))
;           else
;           (car xs)))
(defun instIsConnectedOpin (inst)
       (orList
         (mapcar (lambda (x) (isOpin (car x~>net~>pins~>fig)))
                 (inst~>instTerms)
                 )))

testDatAllLargeScaleAdp()

