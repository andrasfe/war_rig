```cobol
       4000-FILE-CLOSE.                                                 02730000
            DISPLAY 'CLOSING THE FILE'.                                 02740000
      *     CLOSE OPFILE1.                                              02750000
      *                                                                 02760000
      *     IF WS-OUTFL1-STATUS =  SPACES OR '00'                       02770000
      *      CONTINUE                                                   02780000
      *     ELSE                                                        02790000
      *      DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-OUTFL1-STATUS       02800000
      *     END-IF.                                                     02810000
      *     CLOSE OPFILE2.                                              02820000
      *                                                                 02830000
      *     IF WS-OUTFL2-STATUS =  SPACES OR '00'                       02840000
      *      CONTINUE                                                   02850000
      *     ELSE                                                        02860000
      *      DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-OUTFL2-STATUS       02870000
      *     END-IF.                                                     02880000
       4000-EXIT.                                                       02890000
            EXIT.                                                       02900000
```
