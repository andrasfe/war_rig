       4000-FILE-CLOSE.                                                 02630030
            DISPLAY 'CLOSING THE FILE'                                  02631043
            CLOSE OPFILE1.                                              02640034
                                                                        02650030
            IF WS-OUTFL1-STATUS =  SPACES OR '00'                       02660034
             CONTINUE                                                   02670030
            ELSE                                                        02680034
             DISPLAY 'ERROR IN CLOSING 1ST FILE:'WS-OUTFL1-STATUS       02690030
            END-IF.                                                     02700034
            CLOSE OPFILE2.                                              02710034
                                                                        02720030
            IF WS-OUTFL2-STATUS =  SPACES OR '00'                       02730034
             CONTINUE                                                   02740030
            ELSE                                                        02750034
             DISPLAY 'ERROR IN CLOSING 2ND FILE:'WS-OUTFL2-STATUS       02760030
            END-IF.                                                     02770034
       4000-EXIT.                                                       02780030
            EXIT.                                                       02790030
