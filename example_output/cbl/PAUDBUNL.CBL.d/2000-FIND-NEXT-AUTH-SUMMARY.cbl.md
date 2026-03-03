```cobol
      *                                                                 01970026
       1000-EXIT.                                                       01980026
            EXIT.                                                       01990026
      *                                                                 02000026
      *----------------------------------------------------------------*02010026
       2000-FIND-NEXT-AUTH-SUMMARY.                                     02020026
      *----------------------------------------------------------------*02030026
      *                                                                 02040026
      *     DISPLAY 'IN 2000 READ ROOT SEGMENT PARA'                    02041057
      *              PAUT-PCB-STATUS                                    02065050
            INITIALIZE PAUT-PCB-STATUS                                  02066047
            CALL 'CBLTDLI'            USING  FUNC-GN                    02070034
                                        PAUTBPCB                        02080029
                                        PENDING-AUTH-SUMMARY            02090029
                                        ROOT-UNQUAL-SSA.                02100029
      *     DISPLAY ' *******************************'                  02130057
      *     DISPLAY ' AFTER THE ROOT SEG IMS CALL    '                  02130157
      *     DISPLAY 'SEG LEVEL: ' PAUT-SEG-LEVEL                        02132057
      *     DISPLAY 'PCB STATU: ' PAUT-PCB-STATUS                       02133057
      *     DISPLAY 'SEG NAME   : ' PAUT-SEG-NAME                       02135057
      *     DISPLAY ' *******************************'                  02138043
               IF PAUT-PCB-STATUS = SPACES                              02140029
      *             SET NOT-END-OF-AUTHDB TO TRUE                       02160050
                    ADD 1                 TO WS-NO-SUMRY-READ           02170026
                    ADD 1                 TO WS-AUTH-SMRY-PROC-CNT      02180026
                    MOVE PENDING-AUTH-SUMMARY TO OPFIL1-REC             02190030
                    INITIALIZE ROOT-SEG-KEY                             02190156
                    INITIALIZE CHILD-SEG-REC                            02190256
                    MOVE PA-ACCT-ID           TO ROOT-SEG-KEY           02190356
```
