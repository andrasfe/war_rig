```cobol
       2100-INSERT-ROOT-SEG.                                            02043653
                                                                        02043753
            CALL 'CBLTDLI'       USING  FUNC-ISRT                       02070053
                                        PAUTBPCB                        02080029
                                        PENDING-AUTH-SUMMARY            02090029
                                        ROOT-UNQUAL-SSA.                02100053
            DISPLAY ' *******************************'                  02130040
      *     DISPLAY ' AFTER THE ROOT SEG INSERT CALL '                  02130167
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02133067
      *     DISPLAY 'SEG NAME : ' PAUT-SEG-NAME                         02135067
            DISPLAY ' *******************************'                  02138053
            IF PAUT-PCB-STATUS = SPACES                                 02140053
               DISPLAY 'ROOT INSERT SUCCESS    '                        02160053
            END-IF                                                      02191053
            IF PAUT-PCB-STATUS = 'II'                                   02192053
               DISPLAY 'ROOT SEGMENT ALREADY IN DB'                     02194153
            END-IF                                                      02197053
            IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'            02200053
                  DISPLAY 'ROOT INSERT FAILED  :' PAUT-PCB-STATUS       02230053
                  PERFORM 9999-ABEND                                    02260053
            END-IF                                                      02270053
            .                                                           02271053
```
