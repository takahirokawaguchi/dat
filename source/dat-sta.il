;(load "~/git/dat/source/dat-sta.il")
(defun staticTimingAnalysis (scv) 
  (let (staField staTypeField staFilteringField slackListOrMin
        logicCells orderITs ofp fileName propArvTime 
                   delayTable minCP nameMinCP minSlack nameMinSlack)
    ;global variable -> local
    (setq staField DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staField~>value)
    (setq staTypeField DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staTypeField~>value)
    (setq staFilteringField DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>staFilteringField~>value)
    (setq slackListOrMin DISPLAY_ARRIVAL_TIME_OPTIONS_FORM~>slackListOrMin~>value)
    ;file
    (sprintf fileName "timingslack_%s_%s_%d_%s.csv"
             scv~>cellName scv~>cellViewType
             scv~>currentBias staTypeField)
    (setq ofp (outfile fileName "w"))
    (fprintf ofp "cellName,name,order,slack,clockPeriod,difPTL\n")

    ;initialize
    (setq logicCells (setof x scv~>instances 
                            (and
                              (geqp (length (inputTerms x)) 2)
                              (null (member (logicCellType x)
                                            '(;"cb"
                                              "spljtlx" "spl3jtlx"
                                              ;"source"
                                              ;"cbjtlx"
                                              "jtlsink" "jtlx" "jtlxx"))))
                            ))

    (cond
      ((equal staTypeField "max")
       (setq propArvTime1 "arvTimeMax")
       (setq propArvTime2 "arvTimeMax"))
      ((equal staTypeField "min")
       (setq propArvTime1 "arvTimeMin")
       (setq propArvTime2 "arvTimeMin"))
      ((equal staTypeField "minmax")
       (setq propArvTime1 "arvTimeMin")
       (setq propArvTime2 "arvTimeMax"))
      )
    (setq delayTable (makeDelayTable scv))
    (setq minCP 0.0)
    (setq minSlack 1000)
    (setq nameMinSlack "")
    (setq nameMinCP "")

    ;main
    (foreach
      lc logicCells
      (let (slackList perm)
        (setq slackList nil)
        ;calcSlack of cb
        (when (or (haveMaxTimingConstraint lc)
                  (isCBJTLX lc))
          (if (isCBJTLX lc)
            (setq perm (permutation 2 (setof x (inputTerms lc)
                                             (or (equal x~>name "a")
                                                 (equal x~>name "b")))))
            (setq perm (permutation 2 (inputTerms lc))))
          (foreach its perm
                   (setq slackList
                         (append slackList 
                                  (calcSlack
                                    (car its)
                                    (cadr its)
                                    propArvTime1
                                    propArvTime2))))
          ;calcDiffPTL
          ;(when
          ;  (orList (mapcar (lambda (x) (lessp x staFilteringField)) slackList))
          ;  (fprintf ofp "%s %s " lc~>cellName lc~>name)
          ;  (fprintf ofp "%L" slackList)
          ;  (fprintf ofp " minCP=%.1f" (calcMinCP orderITs propArvTime1 propArvTime2))
          ;  (when (or (isKand lc) (isKnimply lc))
          ;    (fprintf ofp " diffPTL=%d"
          ;             (max 
          ;               (abs (car (inputTerms lc))~>numPtlCellMax
          ;                    - (cadr (inputTerms lc))~>numPtlCellMin)
          ;               (abs (cadr (inputTerms lc))~>numPtlCellMax
          ;                    - (car (inputTerms lc))~>numPtlCellMin)
          ;               )))
          ;  (fprintf ofp "\n")
          ;  )
          )
        ;calcSlack of except haveMaxTimingConstraint
        (setq orderITs (orderInputs lc staTypeField))
        ;(setq slackList nil)
        (foreach
          it orderITs
          (when
            (and (null (haveMaxTimingConstraint lc))
                 (nequal it (car (last orderITs))))
            (progn
              (setq slackList
                    (append slackList 
                             (calcSlack
                               it
                               (cadr (member it orderITs))
                               propArvTime1
                               propArvTime2))))))
        ;calc min slack
        (when (not (isCB lc))
          (when slackList
            (when (lessp (minList slackList)
                         minSlack)
              (setq nameMinSlack lc~>name))
            (setq minSlack (min minSlack (minList slackList))))
          );when not CB
        ;calc minCP
        (let (tmp)
          (when  (lessp minCP 
                        (setq tmp (calcMinCP orderITs propArvTime1 propArvTime2)))
            (unless (haveMaxTimingConstraint (car orderITs)~>inst)
              (setq minCP tmp)
              (setq nameMinCP lc~>name))
            ))
        ;default staFilteringField = 0.0
        (when
          (and (andList slackList)
               (or staField!="filtering"
                   (orList (mapcar (lambda (x) (lessp x staFilteringField)) slackList))))
          ;))
          (fprintf ofp "%s,%s," lc~>cellName lc~>name)
          (foreach
            it orderITs
            (fprintf ofp "%s_" it~>name))
          (fileSeek ofp -1 1)
          (if slackListOrMin=="list"
            (fprintf ofp ",%L" slackList)
            (fprintf ofp ",%.1f" (minList slackList))
            )
          (if (haveMaxTimingConstraint (car orderITs)~>inst)
            then
            (fprintf ofp ",")
            else
            (fprintf ofp ",%.1f" (calcMinCP orderITs propArvTime1 propArvTime2)))
          (let (difPTLs temp its it1)
            (setq difPTLs nil)
            (setq its orderITs)
            (while its
                   (setq it1 (car its))
                   (setq its (cdr its))
                   (foreach it2 its
                            (setq difPTLs (append1 difPTLs (abs it1~>numPtlCellMax - it2~>numPtlCellMin)))
                            (setq difPTLs (append1 difPTLs (abs it1~>numPtlCellMin - it2~>numPtlCellMax)))
                            ))
            (fprintf ofp ",%d" (maxList difPTLs))
            )
          (fprintf ofp "\n")
          )))
    (fprintf ofp "minimum timing slack: %s %.1f\n" nameMinSlack minSlack)
    (fprintf ofp "clock period: %s %.1f" nameMinCP minCP)
    (close ofp)
    ))

(defun getInterval
  ( inputTerm1
    inputTerm2
    vb)
  (let (hashkey)
    (setq hashkey (symbolToString (concat (upperCase inputTerm1~>name) "__"
                                          (upperCase inputTerm2~>name) "__"
                                          vb)))
    (delayTable[inputTerm1~>inst~>master][hashkey])))

(defun calcSlack (it next propArvTime1 propArvTime2)
  (if (and (nequal it~>name "clk")
           (nequal next~>name "clk")
           (equal (getInterval it next it~>cellView~>currentBias) 0.0))
    then 
    nil
    else
    (if (haveMaxTimingConstraint it~>inst)
      then
      (list
        (getInterval it next it~>cellView~>currentBias)
        + (dbGetPropByName next propArvTime1)~>value
        - (dbGetPropByName it propArvTime2)~>value)
      else
      (list
        (dbGetPropByName next propArvTime1)~>value
        - (dbGetPropByName it propArvTime2)~>value
        - (getInterval it next it~>cellView~>currentBias))
      )))
(defun calcMinCP (orderITs propArvTime1 propArvTime2)
  (let (fit lit ret)
    ;(setq fit (car orderITs))
    ;(setq lit (car (last orderITs)))
    (setq ret 0.0)
    (foreach
      fit orderITs
      (foreach
        lit orderITs
        (if (nequal 'unbound
                    (getInterval lit fit lit~>cellView~>currentBias))
          then
          (setq ret (max ret
                         (dbGetPropByName lit propArvTime2)~>value
                         - (dbGetPropByName fit propArvTime1)~>value
                         + (getInterval lit fit lit~>cellView~>currentBias)))
          else
          (setq ret (max ret
                         (dbGetPropByName lit propArvTime2)~>value
                         - (dbGetPropByName fit propArvTime1)~>value))
          )))
    ret
    ))

