```cobol
       3200-INSERT-IMS-CALL.                                            02611053
      *                                                                 02611153
      *     DISPLAY 'IN 3200 INSERT CALL'                               02611261
                  CALL 'CBLTDLI' USING  FUNC-ISRT                       02611353
                                        PAUTBPCB                        02611453
                                        PENDING-AUTH-DETAILS            02611553
                                        CHILD-UNQUAL-SSA.               02611654
                                                                        02611753
            IF PAUT-PCB-STATUS = SPACES                                 02611866
               DISPLAY 'CHILD SEGMENT INSERTED SUCCESS'                 02611966
            END-IF                                                      02612055
            IF PAUT-PCB-STATUS = 'II'                                   02612166
               DISPLAY 'CHILD SEGMENT ALREADY IN DB'                    02612266
            END-IF                                                      02612366
            IF PAUT-PCB-STATUS NOT EQUAL TO  SPACES AND 'II'            02612466
                  DISPLAY 'INSERT CALL FAIL FOR CHILD:' PAUT-PCB-STATUS 02612566
                  DISPLAY 'KFB AREA IN CHILD:' PAUT-KEYFB               02612666
                    PERFORM 9999-ABEND                                  02612766
            END-IF.                                                     02612866
                                                                        02612966
       3200-EXIT.                                                       02613066
            EXIT.                                                       02614066
```
