/*
(mload "~/git/dat/source/")
*/
(defun calcArvTimeAll
  ;;inputs
  ( scv 
    @key
    (vb 7)
    (inputTimeMap nil)
    (netDelayCoeff 0.0)
    )
  (let (delayTable inputTermsCalcNext inputTerm inputTermsConnectedSource ipins
                   timingDependencyTable)
    ;; initialization
    (deleteAllArvTime scv)
    (setq delayTable (makeDelayTable scv))
    (setq timingDependencyTable (makeTimingDependencyTable scv))
    (setq ipins (setof y scv~>instances (or (isIpin y) (isOpin y) (isIopin y))))
    (setq inputTermsCalcNext nil)
    (foreach
      x ipins
      (let (y z)
        (when (setq y (assoc x~>net~>name inputTimeMap))
          (progn (car (inputTermsConnected x))~>arvTimeMin = (cadr y)
                 (car (inputTermsConnected x))~>arvTimeMax = (caddr y)
                 ;)
                 (car (inputTermsConnected x))~>numPtlDrvMin=0
                 (car (inputTermsConnected x))~>numPtlDrvMax=0
                 (car (inputTermsConnected x))~>numPtlCellMin=0
                 (car (inputTermsConnected x))~>numPtlCellMax=0)
          ;(progn (car (inputTermsConnected x))~>arvTimeMin=0.0
          ;       (car (inputTermsConnected x))~>arvTimeMax=0.0
          ;       ;)
          ;       (car (inputTermsConnected x))~>numPtlDrvMin=0
          ;       (car (inputTermsConnected x))~>numPtlDrvMax=0
          ;       (car (inputTermsConnected x))~>numPtlCellMin=0
          ;       (car (inputTermsConnected x))~>numPtlCellMax=0)
          )
        (when (setq z (car (inputTermsConnected x)))
          (setq inputTermsCalcNext (cons z inputTermsCalcNext)))
        ))

    (setq inputTermsConnectedSource
          (mapcar (lambda (x) (car (inputTermsConnected x)))
                  (mapcar (lambda (x) (car (outputTerms x)))
                          (setof y scv~>instances (isSource y)))))
    (foreach x inputTermsConnectedSource
             x~>arvTimeMin=10000.0
             x~>arvTimeMax=-10000.0
             x~>numPtlDrvMin=0
             x~>numPtlDrvMax=0
             x~>numPtlCellMin=0
             x~>numPtlCellMax=0
             )
    (setq inputTermsCalcNext (append inputTermsCalcNext inputTermsConnectedSource))

    ;zero skew
    ;(setq inputTermsCalcNext nil)
    ;(foreach ci (setof x scv~>instances (isClocked x))
    ;         (setq inputTermsCalcNext
    ;               (append inputTermsCalcNext
    ;                       (setof x ci~>instTerms (equal x~>name "clk")))))
    ;(foreach x inputTermsCalcNext
    ;         x~>arvTimeMin=0.0
    ;         x~>arvTimeMax=-0.0
    ;         x~>numPtlDrvMin=0
    ;         x~>numPtlDrvMax=0
    ;         x~>numPtlCellMin=0
    ;         x~>numPtlCellMax=0
    ;         )

    ;;procedure
    (while (setq inputTerm (car inputTermsCalcNext))
           (setq inputTermsCalcNext (cdr inputTermsCalcNext))
           (setq inputTermsCalcNext
                 (append
                   inputTermsCalcNext
                   ;(calcArvTimeOutputTerm inputTerm netDelayCoeff)))
                   (calcNumPtl inputTerm netDelayCoeff)))
           )))

(defun calcNumPtl (inputTerm netDelayCoeff)
  (let (ret tmp)
    (setq ret nil)
    (foreach outputTerm (outputTermsDependTiming inputTerm)
             (when (checkCalculability outputTerm)
               ;(andList
               ;  (mapcar (lambda (x) x~>numPtlDrvMin)
               ;          (inputTermsDependTiming outputTerm)))
               ;(when (and (setq tmp (car (inputTermsConnected outputTerm)))
               ;           (null tmp~>numPtlDrvMin))
               ;  (setq ret (cons tmp ret)))
               (when (setq tmp (inputTermsConnected outputTerm))
                 (setq ret (append ret 
                                   (setof x tmp (and x (null x~>numPtlDrvMin))))))
               ;ptl driver
               outputTerm~>numPtlDrvMin =
               (minList (inputTermsDependTiming outputTerm)~>numPtlDrvMin)
               outputTerm~>numPtlDrvMax =
               (maxList (inputTermsDependTiming outputTerm)~>numPtlDrvMax)
               (when (or (isPTLDrv outputTerm~>inst)
                         (isPspl outputTerm~>inst))
                 (progn
                   outputTerm~>numPtlDrvMin = (plus 1 outputTerm~>numPtlDrvMin)
                   outputTerm~>numPtlDrvMax = (plus 1 outputTerm~>numPtlDrvMax)))
               ;ptl cell
               outputTerm~>numPtlCellMin =
               (minList (inputTermsDependTiming outputTerm)~>numPtlCellMin)
               outputTerm~>numPtlCellMax =
               (maxList (inputTermsDependTiming outputTerm)~>numPtlCellMax)
               (when (isPTL outputTerm~>inst)
                 (progn
                   outputTerm~>numPtlCellMin = (plus 1 outputTerm~>numPtlCellMin)
                   outputTerm~>numPtlCellMax = (plus 1 outputTerm~>numPtlCellMax)))
               ;
               (foreach itc ret
                        itc~>numPtlDrvMin  = outputTerm~>numPtlDrvMin
                        itc~>numPtlDrvMax  = outputTerm~>numPtlDrvMax
                        itc~>numPtlCellMin = outputTerm~>numPtlCellMin
                        itc~>numPtlCellMax = outputTerm~>numPtlCellMax)
               (setArvTime outputTerm ret netDelayCoeff)
               ))
    ;return
    ret))


;(defun calcArvTimeOutputTerm (inputTerm netDelayCoeff)
;  (let (ret tmp)
;    ;(printf "%L\n" inputTerm~>inst~>name)
;    (setq ret nil)
;    (foreach outputTerm (outputTermsDependTiming inputTerm)
;             (when (checkCalculability outputTerm)
;               ;(when (and (setq tmp (car (inputTermsConnected outputTerm)))
;               ;           (null tmp~>arvTimeMax))
;               ;  (setq ret (cons tmp ret)))
;               (setq tmp (inputTermsConnected outputTerm))
;               (when (andList (mapcar 'null tmp~>arvTimeMax))
;                 (setq ret (append ret tmp)))
;               (setArvTime outputTerm netDelayCoeff)
;               ));return
;    ret))

(defun checkCalculability (outputTerm)
  (andList
    (mapcar 'isCalculated (inputTermsDependTiming outputTerm))))

(defun setArvTime (outputTerm connectedInputTerms netDelayCoeff)
  (let (inputTerms)
    (setq inputTerms (inputTermsDependTiming outputTerm))
    ;(setq connectedInputTerm (car (inputTermsConnected outputTerm)))
    ;(setq connectedInputTerms (inputTermsConnected outputTerm))
    (foreach connectedInputTerm connectedInputTerms
             ;calculating arvTimeMin
			 if(
				equal(netDelayCoeff "PTLdelay_only")||equal(netDelayCoeff "PTLdelay_only_ideal")
			then
				 outputTerm~>arvTimeMin =
				 (minList (mapcar (lambda (x) (calcArvTimeDiff_zero x~>arvTimeMin x outputTerm))
								  inputTerms))
			else
				 outputTerm~>arvTimeMin =
				 (minList (mapcar (lambda (x) (calcArvTimeDiff x~>arvTimeMin x outputTerm))
								  inputTerms))
			)
             (when connectedInputTerm
               cond(
					(equal(netDelayCoeff "PTLdelay")||equal(netDelayCoeff "PTLdelay_only")
						connectedInputTerm~>arvTimeMin =
					   (plus
						 outputTerm~>arvTimeMin
						 (times 1.0 (netDistance outputTerm connectedInputTerm "PTL_delay"))))
					(equal(netDelayCoeff "PTLdelay_only_ideal")||equal(netDelayCoeff "PTLdelay_ideal")
						connectedInputTerm~>arvTimeMin =
					   (plus
						 outputTerm~>arvTimeMin
						 (plus (times 1.0 (netDistance outputTerm connectedInputTerm "PTL_delay"))
								(times 1.0 (netDistance outputTerm connectedInputTerm "additional_PTL_delay"))
						 )
						)
					)
					(t
					   connectedInputTerm~>arvTimeMin =
					   (plus
						 outputTerm~>arvTimeMin
						 (times netDelayCoeff (netDistance outputTerm connectedInputTerm "manhattan"))))
				)
			 )
             ;calculating arvTimeMax
			 if(
				equal(netDelayCoeff "PTLdelay_only")||equal(netDelayCoeff "PTLdelay_only_ideal")
			then
				outputTerm~>arvTimeMax = 
				 (maxList (mapcar (lambda (x) (calcArvTimeDiff_zero x~>arvTimeMax x outputTerm))
								  inputTerms))
			else
				 outputTerm~>arvTimeMax = 
				 (maxList (mapcar (lambda (x) (calcArvTimeDiff x~>arvTimeMax x outputTerm))
								  inputTerms))
			)
             (when connectedInputTerm
               cond(
					(equal(netDelayCoeff "PTLdelay")||equal(netDelayCoeff "PTLdelay_only")
						connectedInputTerm~>arvTimeMax =
					   (plus
						 outputTerm~>arvTimeMax
						 (times 1.0 (netDistance outputTerm connectedInputTerm "PTL_delay")))
					)
					(equal(netDelayCoeff "PTLdelay_only_ideal")||equal(netDelayCoeff "PTLdelay_ideal")
						connectedInputTerm~>arvTimeMax =
					   (plus
						 outputTerm~>arvTimeMax
						 (plus (times 1.0 (netDistance outputTerm connectedInputTerm "PTL_delay"))
								(times 1.0 (netDistance outputTerm connectedInputTerm "additional_PTL_delay"))
						 )
						)
					)
					(t
					   connectedInputTerm~>arvTimeMax =
					   (plus
						 outputTerm~>arvTimeMax
						 (times netDelayCoeff (netDistance outputTerm connectedInputTerm "manhattan")))
					)
				)
             ))))

(defun netDistance (outputTerm inputTerm netDelayFlag)
  (let (net distance)
    (setq distance 0.0)
    (setq net inputTerm~>net)
    (when net~>figs
      (if (equal netDelayFlag "manhattan")
        then
        (setq distance 
              (plus (abs (difference (xCoord (getInstTermCoord outputTerm))
                                     (xCoord (getInstTermCoord inputTerm))))
                    (abs (difference (yCoord (getInstTermCoord outputTerm))
                                     (yCoord (getInstTermCoord inputTerm))))))
        else
			cond(
				(equal(netDelayFlag "PTL_delay")
					if(
						dbFindProp(net netDelayFlag)==nil
					then
						warn(strcat("cannot find net delay specification the net prop of : " net~>name " : so use manhattan distance"))
						distance = (plus (abs (difference (xCoord (getInstTermCoord outputTerm))
										 (xCoord (getInstTermCoord inputTerm))))
						(abs (difference (yCoord (getInstTermCoord outputTerm))
										 (yCoord (getInstTermCoord inputTerm)))))
					else
						distance = dbFindProp(net netDelayFlag)~>value
					)
				)
				(equal(netDelayFlag "additional_PTL_delay")
					if(
						dbFindProp(net netDelayFlag)==nil
					then
						distance = 0.0
					else
						distance = dbFindProp(net netDelayFlag)~>value
					)
				)
				(t
					(foreach x net~>figs
							 (setq distance 
								   distance
								   + (abs (difference (caar x~>points) (caadr x~>points)))
								   + (abs (difference (cadar x~>points) (cadadr x~>points)))
								   ))
				)
			)
		))
    distance))


(defun calcArvTimeDiff (arvTime inputTerm outputTerm)
  (plus arvTime (getDelay inputTerm outputTerm vb)))
(defun calcArvTimeDiff_zero (arvTime inputTerm outputTerm)
  (plus arvTime 0.0))

(defun readVFile (master)
  (let (in tmp delayTable)
    (setq in (infile (strcat (ddGetObjReadPath master~>lib)
                             "/verilog/"
                             master~>cellName
                             ".v")))
    (when in
      (setq delayTable (makeTable (gensym 'asocList))) 
      (while (gets tmp in)
             (when (rexMatchp "^[ \\t]*parameter" tmp)
               (setq tmp (parseString tmp "=; "))
               (delayTable[(nth 1 tmp)] = (evalstring (nth 2 tmp)))))
      (close in)
      (setq in nil))
    delayTable))

(defun getCellTimingDependency (master)
  (let (in tmp in_out list_in_out inputTable outputTable timingDependencyTable)
    (setq in (infile (strcat (ddGetObjReadPath master~>lib)
                             "/verilog/"
                             master~>cellName
                             ".v")))
    (setq list_in_out nil)
    (when in
      (while (gets tmp in)
             (when (rexMatchp "^[ \\t]*parameter" tmp)
               (setq tmp (parseString tmp "=; "))
               (setq in_out (parseString (nth 1 tmp) "_"))
               ;(printf "%L %L\n" master~>cellName (length in_out))
               (when (equal (length in_out) 5)
                 (setq in_out
                       (list 
                         (strcat (nth 0 in_out) "_" (nth 1 in_out))
                         (strcat (nth 2 in_out) "_" (nth 3 in_out))
                         (nth 4 in_out))) 
                 ;(printf "%L\n" in_out)
                 )
               (when (equal (length in_out) 3)
                 (setq list_in_out (cons (list (car in_out) (cadr in_out))
                                         list_in_out
                                         )))))
      (close in)
      (setq in nil))
    (setq list_in_out (setOfList list_in_out))
    (setq list_in_out (mapcar (lambda (x) (list (lowerCase (car x))
                                                (lowerCase (cadr x))))
                              list_in_out))
    (setq list_in_out (setof x list_in_out (orList
                                             (mapcar (lambda (y) (equal (cadr x)
                                                                        y))
                                                     (masterOutputTerminals master)~>name))))
    (setq inputTable (setOfList (mapcar 'car list_in_out)))
    (setq inputTable 
          (mapcar (lambda (x)
                    (list x
                          (mapcar 'cadr (setof y list_in_out (equal x (car y))))))
                  inputTable))
    (setq outputTable (setOfList (mapcar 'cadr list_in_out)))
    (setq outputTable 
          (mapcar (lambda (x)
                    (list x
                          (mapcar 'car (setof y list_in_out (equal x (cadr y))))))
                  outputTable))
    (setq timingDependencyTable (makeTable (gensym 'asocList)))
    (foreach x (append inputTable outputTable)
             (timingDependencyTable[(car x)] = (cadr x)))
    timingDependencyTable
    ))

(defun makeTimingDependencyTable (scv)
  (let (tmp timingDependencyTable)
    ;(printf "number of masters: %d\n" (length scv~>instanceMasters))
    (setq timingDependencyTable (makeTable (gensym 'asocList)))
    (foreach m scv~>instanceMasters
             (setq tmp (getCellTimingDependency m))
             (if tmp
               (progn
                 (timingDependencyTable[m] = tmp)
                 ;(printf "%L\n" timingDependencyTable[m])
                 ;(printf "%L %L:" m~>cellName (car m~>terminals)~>name)
                 ;(when (nequal timingDependencyTable[m][(car m~>terminals)~>name] 'unbound)
                 ;  (printf "%L\n" timingDependencyTable[m][(car m~>terminals)~>name])
                 ;  )
                 )
               (warn "can't read delays of %s %s. @makeTimingDependencyTable\n" m~>cellName m~>libName)))
    timingDependencyTable
    ))
(defun masterOutputTerminals (master)
  (setof x master~>terminals (equal x~>direction "output")))

(defun makeDelayTable (scv)
  (let (delayTable tmp)
    (setq delayTable (makeTable (gensym 'asocList)))
    (foreach m (setof x scv~>instanceMasters
                      (nequal x~>libName "basic"))
             (setq tmp (readVFile m))
             (if tmp
               (delayTable[m] = tmp)
               (warn "can't read delays of %s %s. @makeDelayTable\n" m~>cellName m~>libName)))
    delayTable))

(defun getDelay
  ( inputTerm
    outputTerm
    vb)
  (let (hashkey)
    (when (isPTLFamily inputTerm~>inst)
      (setq vb 1))
    (setq hashkey (symbolToString (concat (upperCase inputTerm~>name) "__"
                                          (upperCase outputTerm~>name) "__"
                                          vb)))
    (if
      (equal 
        (delayTable[inputTerm~>inst~>master][hashkey])
        'unbound
        )
      (warn "can't read delay of %s" inputTerm~>inst~>cellName)
      (delayTable[inputTerm~>inst~>master][hashkey])
      )
    ))

;temp:PTL cell only
;(defun getDelay
;  ( inputTerm
;    outputTerm
;    vb)
;  (let (hashkey)
;    (when (isPTLFamily inputTerm~>inst)
;      (setq vb 1))
;    (setq hashkey (symbolToString (concat (upperCase inputTerm~>name) "__"
;                                          (upperCase outputTerm~>name) "__"
;                                          vb)))
;    (if (isPTLFamily inputTerm~>inst)
;      (delayTable[inputTerm~>inst~>master][hashkey])
;      0.0
;      )))

;;;=======
;;;Utility
;;;=======
(defun inputTermsConnected (pinOrOutputTerm)
  (if (geqp (length (setof x pinOrOutputTerm~>net~>instTerms (isInputTerm x))) 2)
    (cond 
      ((member (ptlType pinOrOutputTerm~>inst)
               (list "PTL2" "viar"))
       (setof x pinOrOutputTerm~>net~>instTerms
              (and
                (isInputTerm x)
                (member (ptlType x~>inst)
                        (list "PTL2" "via")))))
      ((member (ptlType pinOrOutputTerm~>inst)
               (list "PTL1" "via"))
       (setof x pinOrOutputTerm~>net~>instTerms
              (and
                (isInputTerm x)
                (member (ptlType x~>inst)
                        (list "PTL1" "viar")))))
      (t
        (progn
          (warn "%s_%s is connected to more than one inputTerms"
                pinOrOutputTerm~>inst~>name pinOrOutputTerm~>name)
          (setof x pinOrOutputTerm~>net~>instTerms (isInputTerm x))
          )));cond
    );if
  (setof x pinOrOutputTerm~>net~>instTerms (isInputTerm x))
  )
(defun ptlType (inst)
  (cond
    ((rexMatchp "^PTL1" inst~>cellName)
     "PTL1")
    ((or
       (rexMatchp "^PTL2" inst~>cellName)
       (rexMatchp "^drv" inst~>cellName)
       (rexMatchp "^rec" inst~>cellName))
     "PTL2")
    ((rexMatchp "^via[a-z]*r$" inst~>cellName)
     "viar")
    ((rexMatchp "^via[a-z]*" inst~>cellName)
     "via")))
(defun outputTermsConnected (pinOrInputTerm)
  (setof x pinOrInputTerm~>net~>instTerms (isOutputTerm x))) 

(defun isClocked (inst) (setof x inst~>instTerms x~>name == "clk"))
(defun isIpin (inst) inst~>cellName == "ipin")
(defun isOpin (inst) inst~>cellName == "opin")
(defun isIopin (inst) inst~>cellName == "iopin")
(defun isJTL (inst)
  (and (null (isRoutingCell inst))
       (rexMatchp "^jtl" inst~>cellName)))
(defun isSPLJTLX (inst) 
  (or (rexMatchp "^spl.jtl" inst~>cellName)
      (rexMatchp "^spl[a-zA-Z].*jtl" inst~>cellName)))
(defun isSPL3JTLX (inst) (rexMatchp "^spl3.jtl" inst~>cellName))
(defun isCB (inst)
  (and (null (isCBJTLX inst))
       (rexMatchp "^cb" inst~>cellName)
       ))
(defun isKand (inst)
  (rexMatchp "^kand*" inst~>cellName))
(defun isKnimply (inst)
  (rexMatchp "^knimply*" inst~>cellName))
(defun isCBJTLX (inst) (rexMatchp "^cb.jtl" inst~>cellName))
(defun isRoutingCell (inst) 
  (length (setof x inst~>instTerms (isInputTerm x))) == 1)
(defun isPTLFamily (inst) 
  (or
    (isPTL inst)
    (isPTLRec inst)
    ))
(defun isPTLDrvRec (inst) 
  (or
    ;(isPTL inst)
    (isPTLRec inst)
    (isPTLDrv inst)
    (isPspl inst)
    ))
(defun isPTL (inst)
  (or
    (rexMatchp "^PTL" inst~>cellName)
    (rexMatchp "^via" inst~>cellName)
    ))
(defun isPTLRec (inst) 
  (rexMatchp "^rec" inst~>cellName))
(defun isPTLDrv (inst) 
  (rexMatchp "^drv" inst~>cellName))
(defun isPspl (inst) 
  (rexMatchp "^pspl" inst~>cellName))
(defun isSource (inst) (rexMatchp "^source" inst~>cellName))
(defun isT1 (inst) (rexMatchp "^t1" inst~>cellName))
(defun isD2ff (inst) (rexMatchp "^d2ff" inst~>cellName))
(defun isRdff (inst)
  (or (rexMatchp "^rdff" inst~>cellName)
      (rexMatchp "^d3ff" inst~>cellName)))
(defun isRtff (inst) (rexMatchp "^rtff" inst~>cellName))
(defun isDffe (inst) (rexMatchp "^dffe" inst~>cellName))
(defun isDff (inst)
  (and (null (isDffe inst))
       (rexMatchp "^dff" inst~>cellName)))
(defun isNdroc (inst) (rexMatchp "^ndroc" inst~>cellName))
(defun isNdro (inst)
  (and (null (isNdroc inst))
       (or
         (rexMatchp "^ndro" inst~>cellName)
         (rexMatchp "^nrdo" inst~>cellName))
       ))
(defun isTff (inst) (rexMatchp "^tff" inst~>cellName))
(defun isJnote (inst) (rexMatchp "^jnote" inst~>cellName))
(defun isJnot (inst)
  (and (null (isJnote inst))
       (rexMatchp "^jnot" inst~>cellName)))
(defun isJande (inst) (rexMatchp "^jande" inst~>cellName))
(defun isJandf (inst) (rexMatchp "^jandf" inst~>cellName))
(defun isJand (inst)
  (and (null (isJande inst))
       (null (isJandf inst))
       (rexMatchp "^jand" inst~>cellName)))
(defun isJnand (inst) (rexMatchp "^jnand" inst~>cellName))
(defun isJor (inst) (rexMatchp "^jor" inst~>cellName))
(defun isJnor (inst) (rexMatchp "^jnor" inst~>cellName))
(defun isJxore (inst) (rexMatchp "^jxore" inst~>cellName))
(defun isJxor (inst)
  (and (null (isJxore inst))
       (rexMatchp "^jxor" inst~>cellName)))
(defun isRjxor (inst) (rexMatchp "^rjxor" inst~>cellName))
;(defun is2x2join (inst) (rexMatchp "^join2x2" inst~>cellName))
;(defun isBina (inst) (rexMatchp "^Bina" inst~>cellName))
;(defun isTBina (inst) (rexMatchp "^tBina" inst~>cellName))
;(defun isMullerc (inst) (rexMatchp "^mullerc" inst~>cellName))
;(defun is (inst) (rexMatchp "^" inst~>cellName))

(defun haveMaxTimingConstraint (inst)
  (or (isCB inst)
      (isKand inst)
      (isKnimply inst)))


(defun inputTerms (inst) (setof x inst~>instTerms (isInputTerm x)))
(defun outputTerms (inst) (setof x inst~>instTerms (isOutputTerm x)))

(defun isInputTerm (instTerm) instTerm~>direction == "input")
(defun isOutputTerm (instTerm) instTerm~>direction == "output")
(defun isCalculated (instTerm)
  (and instTerm~>arvTimeMin instTerm~>arvTimeMax))
(defun pinsConnected (instTerm) (car instTerm~>net~>pins~>figs))


(defun outputTermsDependTiming (inputTerm)
  ;(printf "%L\n" inputTerm~>inst~>cellName)
  ;(printf "%L\n" timingDependencyTable[inputTerm~>inst~>master][inputTerm~>name])
  (if (nequal timingDependencyTable[inputTerm~>inst~>master][inputTerm~>name] 'unbound)
    (setof x (inputTerm~>inst~>instTerms)
           (orList
             (mapcar (lambda (y) (equal x~>name y))
                     timingDependencyTable[inputTerm~>inst~>master][inputTerm~>name])))
    ;(warn "%L of %L does not exist in timingDependencyTable"
    ;      inputTerm~>name
    ;      inputTerm~>inst~>cellName)
    nil
    ))
;(let (inst)
;  (setq inst inputTerm~>inst)
;  (cond ;function's return is cond's return.
;    ((and (isJandf inst)
;          (or (inputTerm~>name=="clk")
;              (inputTerm~>name=="b")))
;     (setof x inst~>instTerms x~>name=="c"))
;    ((or (isRoutingCell inst)
;         (and (isClocked inst)
;              (inputTerm~>name=="clk"))
;         (and (isRdff inst)
;              (inputTerm~>name=="clock"))
;         (and (isRtff inst)
;              (inputTerm~>name=="din"))
;         (haveMaxTimingConstraint inst))
;     (setof x inst~>instTerms x~>direction=="output"))
;    ((isJTL inst)
;     (setof x inst~>instTerms
;            x~>name==(exchangeDinDout inputTerm~>name)))
;    ((isD2ff inst)
;     (setof x inst~>instTerms
;            x~>name==(exchangeClkOut inputTerm~>name)))
;    ((isSPLJTLX inst)
;     (cond (inputTerm~>name=="a"
;             (setof x inst~>instTerms 
;                    (or x~>name=="b" x~>name=="c")))
;           (inputTerm~>name=="d"
;             (setof x inst~>instTerms x~>name=="e"))
;           ))
;    ((isSPL3JTLX inst)
;     (cond (inputTerm~>name=="a"
;             (setof x inst~>instTerms 
;                    (or x~>name=="b" x~>name=="c" x~>name=="d")))
;           (inputTerm~>name=="e"
;             (setof x inst~>instTerms x~>name=="f"))
;           ))
;    ((isCBJTLX inst)
;     (cond ((or inputTerm~>name=="a"
;                inputTerm~>name=="b")
;            (setof x inst~>instTerms x~>name=="c"))
;           (inputTerm~>name=="d"
;             (setof x inst~>instTerms x~>name=="e"))
;           ))
;    ((isT1 inst)
;     (cond (inputTerm~>name=="clock"
;             (setof x inst~>instTerms x~>name=="sum"))
;           (inputTerm~>name=="data"
;             (setof x inst~>instTerms x~>name=="carry"))
;           ))
;    )))

(defun inputTermsDependTiming (outputTerm)
  (if (equal timingDependencyTable[outputTerm~>inst~>master][outputTerm~>name] 'unbound)
    nil
    (setof x (outputTerm~>inst~>instTerms)
           (orList
             (mapcar (lambda (y) (equal x~>name y))
                     timingDependencyTable[outputTerm~>inst~>master][outputTerm~>name])))))
;  (let (inst)
;    (setq inst outputTerm~>inst)
;    (cond ;function's return is cond's return.
;      ((or (isRoutingCell inst)
;           (haveMaxTimingConstraint inst))
;       (setof x inst~>instTerms x~>direction=="input"))
;      ((isJandf inst)
;       (setof x inst~>instTerms 
;              (or x~>name=="b"
;                  x~>name=="clk")
;              ))
;      ((isClocked inst)
;       (setof x inst~>instTerms x~>name=="clk"))
;      ((isRdff inst)
;       (setof x inst~>instTerms x~>name=="clock"))
;      ((isRtff inst)
;       (setof x inst~>instTerms x~>name=="din"))
;      ((isJTL inst)
;       (setof x inst~>instTerms
;              x~>name==(exchangeDinDout outputTerm~>name)))
;      ((isD2ff inst)
;       (setof x inst~>instTerms
;              x~>name==(exchangeClkOut outputTerm~>name)))
;      ((isSPLJTLX inst)
;       (cond ((or outputTerm~>name=="b"
;                  outputTerm~>name=="c")
;              (setof x inst~>instTerms x~>name=="a"))
;             (outputTerm~>name=="e"
;               (setof x inst~>instTerms x~>name=="d"))
;             ))
;      ((isSPL3JTLX inst)
;       (cond ((or outputTerm~>name=="b"
;                  outputTerm~>name=="c"
;                  outputTerm~>name=="d")
;              (setof x inst~>instTerms x~>name=="a"))
;             (outputTerm~>name=="f"
;               (setof x inst~>instTerms x~>name=="e"))
;             ))
;      ((isCBJTLX inst)
;       (cond ( outputTerm~>name=="c"
;               (setof x inst~>instTerms 
;                      (or x~>name=="a" x~>name=="b")))
;             ( outputTerm~>name=="e"
;               (setof x inst~>instTerms x~>name=="d"))
;             ))
;      ((isT1 inst)
;       (cond ( outputTerm~>name=="sum"
;               (setof x inst~>instTerms x~>name=="clock" ))
;             (outputTerm~>name=="carry"
;               (setof x inst~>instTerms x~>name=="data"))
;             ))
;      )))


(defun exchangeDinDout (string)
  (cond
    ((equal (strncmp string "din" 3) 0)
     (setq x (strcat "dout" (substring string 4))))
    ((equal (strncmp string "dout" 4) 0)
     (strcat "din" (substring string 5)))
    ))
(defun exchangeClkOut (string)
  (cond
    ((equal (strncmp string "clk" 3) 0)
     (setq x (strcat "out" (substring string 4))))
    ((equal (strncmp string "out" 3) 0)
     (strcat "clk" (substring string 4)))
    ))

(defun andList (xs)
  (if (cdr xs)
    (and (car xs)
         (andList (cdr xs)))
    (car xs)))
(defun orList (xs)
  (if (cdr xs)
    (or (car xs)
        (orList (cdr xs)))
    (car xs)))
(defun minList (xs)
  (if (cdr xs)
    (min  (car xs) (minList (cdr xs)))
    (car xs)))
(defun maxList (xs)
  (if (cdr xs)
    (max  (car xs) (maxList (cdr xs)))
    (car xs)))

(defun deleteAllArvTime (scv)
  (foreach x scv~>instances
           (foreach y x~>instTerms
                    (dbDeletePropByName y "arvTimeMin") 
                    (dbDeletePropByName y "arvTimeMax")
                    (dbDeletePropByName y "numPtlDrvMin") 
                    (dbDeletePropByName y "numPtlDrvMax") 
                    (dbDeletePropByName y "numPtlCellMin") 
                    (dbDeletePropByName y "numPtlCellMax") 
                    ))) 


