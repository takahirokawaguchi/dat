;(mload "~/git/dat/source")
; DISPLAY_ARRIVAL_TIME_CONST
(defvar DISPLAY_ARRIVAL_TIME_CONST)
(setplist 'DISPLAY_ARRIVAL_TIME_CONST
          '(
            INIT_ARV_TIME -1.0
            ;LABEL_LAYER ("y3" "drawing")
            LABEL_LAYER ("y4" "drawing")
            LABEL_LAYER_STA_HOLDNG ("y4" "drawing")
            LABEL_LAYER_STA_SETUPNG ("y6" "drawing")
            DEFAULT_VB_LEVEL 7
            DEFAULT_VB_LEVEL_PTL 1
            PTL_FAMILY_REX ("^PTL*" "^via*" "^rec*")
            ))

(defun datGui ()
  (let (scv paramaList ok)
    (setq scv (geGetWindowCellView (hiGetCurrentWindow)))
    ;オプションフォームのオブジェクトを格納しておく変数
    ;(defvar DISPLAY_ARRIVAL_TIME_OPTIONS_FORM)
    (unless (boundp 'DISPLAY_ARRIVAL_TIME_OPTIONS_FORM)
      (createOptionsForm scv)
      )
    (setq ok
          hiDisplayForm( DISPLAY_ARRIVAL_TIME_OPTIONS_FORM ))

    (when ok
      ;paramList
      ;  '(imap '('(i0, 0)...)
      ;    vb 7
      ;    flagSTA "off"
      ;    flagArvTime "minmax"
      ;    flagFiltering 0.0
      ;    flagRemoving nil
      ;  )
      (setq paramList (getParamsFromOptionsForm ))
      println(paramList)
      ;;my_paramList = paramList
      (displayArrivalTime scv ?paramList paramList)
      )
    ))

(defun displayArrivalTime (scv @key (paramList nil))
  (let (staCP staField)
    (removeAllLabels scv)
    (unless (cadddr (cdddr paramList))
      (calcArvTimeAll scv
                      ?vb (caddr paramList)
                      ?inputTimeMap (car paramList)
                      ?netDelayCoeff (cadr paramList))
      (glueLabelsToInst_inout scv)
      scv~>currentBias = (caddr paramList)
      )
    (setq staField DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staField~>value)
    (when (nequal staField "off")
      (staticTimingAnalysis scv))
    ))
(defun writeArrivalTime (scv @key (paramList nil))
  (let (staCP)
    (unless (cadddr (cdddr paramList))
      (calcArvTimeAll scv
                      ?vb (caddr paramList)
                      ?inputTimeMap (car paramList)
                      ?netDelayCoeff (cadr paramList))
      )))

(defun getParamsFromOptionsForm ()
  (let (result)
    ;; inputTimeMapのチェックと取得
    (let (scv fileName in out temp imap str)
      (setq scv (geGetWindowCellView ))
      (sprintf fileName "inputTimeMap_%s.txt" scv~>cellName)
      (setq in (infile fileName))
      (if in
        (progn;then
          (setq imap nil)
          (while (setq temp (lineread in))
                 (setq temp
                       (cons
                         (sprintf str "%s" (car temp))
                         (cdr temp)))
                 (setq imap (cons temp imap)))
          (setq result (list imap)))
        (progn;else
          (setq out (outfile fileName "w"))
          (setq imap nil)
          (foreach x (setof y scv~>instances (or (isOpin y) (isIpin y) (isIopin y)))
                   (fprintf out "%s %.1f %.1f\n" x~>net~>name 0.0 0.0)
                   (setq imap (append1 imap (list x~>net~>name 0.0 0.0))) 
                   )
          (close out)
          (setq result (list imap))
          )));let
    if(
       strcmp(DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>netDelayCoeffField~>value "PTLdelay")==0||
       strcmp(DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>netDelayCoeffField~>value "PTLdelay_only")==0||
       strcmp(DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>netDelayCoeffField~>value "PTLdelay_only_ideal")==0||
       strcmp(DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>netDelayCoeffField~>value "PTLdelay_ideal")==0
       then
       (setq result (append1 result DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>netDelayCoeffField~>value))
       else
       (setq result (append1 result (evalstring DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>netDelayCoeffField~>value)))
       )
    ;; VBのチェックと取得
    (let (vb)
      (cond
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "-30%" (setq vb 1))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "-25%" (setq vb 2))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "-20%" (setq vb 3))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "-15%" (setq vb 4))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "-10%" (setq vb 5))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "-5%" (setq vb 6))	 
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "0%" (setq vb 7))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "+5%" (setq vb 8))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "+10%" (setq vb 9))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "+15%" (setq vb 10))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "+20%" (setq vb 11))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "+25%" (setq vb 12))
        (DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>vbToggleField~>value == "+30%" (setq vb 13)))
      (setq result (append1 result vb)))
    ;; flagSTA
    (setq result (append1 result DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staField~>value))
    ;;flagArvTime
    (setq result (append1 result
                          DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staTypeField~>value
                          ))
    ;;flagFiltering
    (setq result (append1 result
                          DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staFilteringField~>value
                          ))
    ;; removingのチェックと取得
    (if DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>isRemoveField~>value == "off"
      (setq result (append1 result nil))
      (setq result (append1 result t)))
    ;(warn "[DEBUG] #getParamsFromOptionsForm params:%L\n" result)
    result))

(defun createOptionsForm (scv)
  (let (comps)
    (setq comps (append1 comps (hiCreateStringField ?name 'netDelayCoeffField
                                                    ?prompt "delay coefficient for wire length (default: ptl delay / cell width)\n or PTLdelay specification (PTLdelay)\n or (PTLdelay_only)\n or (PTLdelay_only_ideal)\n or (PTLdelay_ideal)"
                                                    ?value "0.1597 / 0.1875"
                                                    ?editable t)))
    (setq comps (append1 comps (hiCreateCyclicField ?name 'vbToggleField
                                                    ?choices (list "-30%" "-25%" "-20%" "-15%" "-10%" "-5%" "0%" "+5%" "+10%" "+15%" "+20%" "+25%" "+30%")
                                                    ?prompt "bias voltage"
                                                    ?value "0%"
                                                    )))
    (setq comps (append1 comps (hiCreateSeparatorField ?name 'separator1)))
    (setq comps (append1 comps (hiCreateRadioField ?name 'staField
                                                   ?choices (list "off" "on" "filtering")
                                                   ?prompt "static timing analysis"
                                                   ?value "off")))
    (setq comps (append1 comps (hiCreateRadioField ?name 'staTypeField
                                                   ?choices (list "max" "min" "minmax")
                                                   ?prompt "sta type"
                                                   ?value "minmax")))
    (setq comps (append1 comps (hiCreateFloatField ?name 'staFilteringField
                                                   ?value 0.0
                                                   ?prompt "filtering timing slack"
                                                   )))
    (setq comps (append1 comps (hiCreateRadioField ?name 'slackListOrMin
                                                   ?choices (list "list" "min")
                                                   ?prompt "slack list or minimum slack per cell"
                                                   ?value "list")))
    (setq comps (append1 comps (hiCreateSeparatorField ?name 'separator2)))
    (setq comps (append1 comps (hiCreateRadioField ?name 'isRemoveField
                                                   ?choices (list "off" "on")
                                                   ?prompt "remove all glued labels"
                                                   ?value "off")))
    (hiCreateAppForm
      ?name 'DISPLAY_ARRIVAL_TIME_OPTIONS_FORM
      ;;(sprintf nil "Display arrival times. lib:%s cell:%s" scv~>libName scv~>cellName)
      ;; オプションフォームの内部コンポーネントのリスト
      ?fields comps
      ;; ヘルプ
      ?help "This is a temporary help message.")))



;ラベル関係の関数
;========================
;glueLabelsToInst
;getInstTermCoord
;upperCenter
;removeAllLabelsFromInst
;========================
(defun glueLabelsToInst (scv)
  (foreach inst scv~>instances
           (foreach x (outputTerms inst)
                    ;(when (and x~>arvTimeMin
                    (when (and (and x~>arvTimeMin x~>numPtlDrvMin)
                               (or 
                                 (null (isPTLFamily inst))
                                 (isPTLRec inst)))
                      (glueLabelsToInstTerm x (list x~>arvTimeMin x~>numPtlDrvMin x~>numPtlCellMin) "upperCenter")
                      (glueLabelsToInstTerm x (list x~>arvTimeMax x~>numPtlDrvMax x~>numPtlCellMax) "lowerCenter")))
           ))
(defun glueLabelsToInst_inout (scv)
  (foreach inst scv~>instances
           (foreach x (outputTerms inst)
                    ;(when (and x~>arvTimeMin
                    (when (and (and x~>arvTimeMin x~>numPtlDrvMin)
                               (or 
                                 (null (isPTLFamily inst))
                                 (isPTLRec inst)))
                      (glueLabelsToInstTerm x (list x~>arvTimeMin x~>numPtlDrvMin x~>numPtlCellMin) "upperCenter")
                      (glueLabelsToInstTerm x (list x~>arvTimeMax x~>numPtlDrvMax x~>numPtlCellMax) "lowerCenter")))
           (foreach x (inputTerms inst)
                    ;(when (and x~>arvTimeMin
                    (when (and (and x~>arvTimeMin x~>numPtlDrvMin)
                               (or 
                                 (null (isPTLFamily inst))
                                 (isPTLRec inst)))
                      (glueLabelsToInstTerm x (list x~>arvTimeMin x~>numPtlDrvMin x~>numPtlCellMin) "upperCenter")
                      (glueLabelsToInstTerm x (list x~>arvTimeMax x~>numPtlDrvMax x~>numPtlCellMax) "lowerCenter")))
           ))					  

(defun glueLabelsToInstTerm (instTerm arvTime position)
  (let (label)
    (setq label (dbCreateLabel instTerm~>inst~>cellView 
                               DISPLAY_ARRIVAL_TIME_CONST.LABEL_LAYER
                               (getInstTermCoord instTerm)
                               ;(sprintf nil "%.1f" arvTime)
                               (sprintf nil "%.1f\n%d/%d" (car arvTime) (cadr arvTime) (caddr arvTime))
                               position
                               "R0"
                               "roman"
                               ;0.0625))
                               0.03125))
    (schGlueLabel label instTerm~>inst)
    (label~>instTermName = instTerm~>name)))

(defun getInstTermCoord (instTerm)
  let((masterTerm masterTermXY)
      cond(
           ; check object type
           (instTerm~>objType != "instTerm"
                              warn("calcInstTermCoord: object \"%L\" not instTerm."
                                   instTerm))

           ; get coordinates from master cell view
           (null((masterTerm = car(exists(x instTerm~>inst~>master~>terminals
                                            x~>name == instTerm~>name)))
                 && (masterTermXY = centerBox(car(masterTerm~>pins)~>fig~>bBox)))
             warn("calcInstTermCoord: cannot get pin \"%s\" coordinate."
                  instTerm~>name))

           ; calculate
           (t
             dbTransformPoint(masterTermXY
                               list(instTerm~>inst~>xy instTerm~>inst~>orient))))))

(defun upperCenter (bBox)
  (list (((caar bBox) + (caadr bBox)) / 2) (cadadr bBox)))

(defun removeAllLabels (scv)
  (foreach instance scv~>instances
           (foreach child instance~>children
                    (when child~>objType == "label" && child~>instTermName != nil
                      (dbDeleteObject child))))
  (deleteAllArvTime scv))
