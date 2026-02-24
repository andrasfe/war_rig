```cobol
       3100-INSERT-CHILD-SEG.                                           02359753
      *                                                                 02360026
      *     DISPLAY 'IN 3100 INSERT CHILD SEG PARA'                     02361061
            INITIALIZE PAUT-PCB-STATUS                                  02362058
            CALL 'CBLTDLI'       USING  FUNC-GU                         02370053
                                        PAUTBPCB                        02380030
                                        PENDING-AUTH-SUMMARY            02390053
                                        ROOT-QUAL-SSA.                  02400053
               DISPLAY '***************************'                    02401043
      *        DISPLAY ' AFTER ROOT SEG GU CALL    '                    02402067
      *        DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                    02410067
      *        DISPLAY 'SEG NAME : ' PAUT-SEG-NAME                      02411067
               DISPLAY '***************************'                    02412043
               IF PAUT-PCB-STATUS = SPACES                              02420030
                  DISPLAY 'GU CALL TO ROOT SEG SUCCESS'                 02430053
      *           ADD 2 TO PA-AUTH-DATE-9C                              02430167
      *           ADD 2 TO PA-AUTH-TIME-9C                              02430267
                  PERFORM 3200-INSERT-IMS-CALL  THRU 3200-EXIT          02430353
               IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'         02520059
                  DISPLAY 'ROOT GU CALL FAIL:' PAUT-PCB-STATUS          02530053
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02531048
                    PERFORM 9999-ABEND                                  02540049
               END-IF.                                                  02550051
       3100-EXIT.                                                       02590053
            EXIT.                                                       02600026
```
