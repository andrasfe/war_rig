```cobol
       4000-FILE-CLOSE.                                                 02630030
            DISPLAY 'CLOSING THE FILE'                                  02631043
            CLOSE INFILE1.                                              02640053
                                                                        02650030
            IF WS-INFIL1-STATUS =  SPACES OR '00'                       02660053
             CONTINUE                                                   02670030
            ELSE                                                        02680034
             DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-INFIL1-STATUS       02690053
            END-IF.                                                     02700034
            CLOSE INFILE2.                                              02710053
                                                                        02720030
            IF WS-INFIL2-STATUS =  SPACES OR '00'                       02730053
             CONTINUE                                                   02740030
            ELSE                                                        02750034
             DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-INFIL2-STATUS       02760053
            END-IF.                                                     02770034
       4000-EXIT.                                                       02780030
            EXIT.                                                       02790030
```
