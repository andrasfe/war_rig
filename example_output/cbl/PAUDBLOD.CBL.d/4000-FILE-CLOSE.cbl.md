```cobol
            .                                                           02271053
       2100-EXIT.                                                       02280053
            EXIT.                                                       02290053
      *                                                                 02310026
      *                                                                 02320026
      *----------------------------------------------------------------*02330026
       3000-READ-CHILD-SEG-FILE.                                        02340053
      *----------------------------------------------------------------*02350026
      *     DISPLAY 'IN 3000 READ CHILD SEG FILE PARA'                  02351067
            READ INFILE2                                                02352053
                                                                        02353053
            IF WS-INFIL2-STATUS =  SPACES OR '00'                       02354053
               IF ROOT-SEG-KEY IS NUMERIC                               02354162
      *        DISPLAY 'GNGTO ROOT SEG KEY'                             02355067
               MOVE ROOT-SEG-KEY  TO QUAL-SSA-KEY-VALUE                 02355260
      *        DISPLAY 'ROOT-SEG-KEY : '    QUAL-SSA-KEY-VALUE          02355367
      *        DISPLAY 'MOVED ROOT SEG KEY'                             02355467
```
