      *                                                                 01350000
      *----------------------------------------------------------------*01360000
       PROCEDURE DIVISION                  USING PAUTBPCB               01370000
                                                 PASFLPCB               01380000
                                                 PADFLPCB.              01381000
      *----------------------------------------------------------------*01390000
      *                                                                 01400000
       MAIN-PARA.                                                       01410000
            ENTRY 'DLITCBL'                 USING PAUTBPCB              01420000
                                                  PASFLPCB              01421000
                                                  PADFLPCB.             01422000
                                                                        01430000
      *                                                                 01440000
           PERFORM 1000-INITIALIZE                THRU 1000-EXIT        01450000
      *                                                                 01460000
           PERFORM 2000-FIND-NEXT-AUTH-SUMMARY    THRU 2000-EXIT        01470000
           UNTIL   WS-END-OF-ROOT-SEG = 'Y'                             01480000
                                                                        01490000
           PERFORM 4000-FILE-CLOSE THRU 4000-EXIT                       01500000
      *                                                                 01510000
      *                                                                 01520000
      *                                                                 01530000
           GOBACK.                                                      01540000
      *                                                                 01550000
      *----------------------------------------------------------------*01560000
       1000-INITIALIZE.                                                 01570000
      *----------------------------------------------------------------*01580000
      *                                                                 01590000
           ACCEPT CURRENT-DATE     FROM DATE                            01600000
           ACCEPT CURRENT-YYDDD    FROM DAY                             01610000
                                                                        01620000
      *    ACCEPT PRM-INFO FROM SYSIN                                   01630000
           DISPLAY 'STARTING PROGRAM DBUNLDGS::'                        01640000
           DISPLAY '*-------------------------------------*'            01650000
           DISPLAY 'TODAYS DATE            :' CURRENT-DATE              01660000
           DISPLAY ' '                                                  01670000
                                                                        01680000
           .                                                            01690000
      *    OPEN OUTPUT OPFILE1                                          01700000
      *    IF WS-OUTFL1-STATUS =  SPACES OR '00'                        01710000
      *       CONTINUE                                                  01720000
      *    ELSE                                                         01730000
      *       DISPLAY 'ERROR IN OPENING OPFILE1:' WS-OUTFL1-STATUS      01740000
      *       PERFORM 9999-ABEND                                        01750000
      *    END-IF                                                       01760000
      *                                                                 01770000
      *    OPEN OUTPUT OPFILE2                                          01780000
      *    IF WS-OUTFL2-STATUS =  SPACES OR '00'                        01790000
      *       CONTINUE                                                  01800000
      *    ELSE                                                         01810000
      *       DISPLAY 'ERROR IN OPENING OPFILE2:' WS-OUTFL2-STATUS      01820000
      *       PERFORM 9999-ABEND                                        01830000
      *    END-IF.                                                      01840000
      *                                                                 01850000
      *                                                                 01860000
       1000-EXIT.                                                       01870000
            EXIT.                                                       01880000
