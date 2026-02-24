```cobol
      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT                   01210000
       01 PENDING-AUTH-SUMMARY.                                         01220000
       COPY CIPAUSMY.                                                   01230000
                                                                        01240000
      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD                  01250000
       01 PENDING-AUTH-DETAILS.                                         01260000
       COPY CIPAUDTY.                                                   01270000
                                                                        01280000
      *                                                                 01290000
      *----------------------------------------------------------------*01300000
       LINKAGE SECTION.                                                 01310000
      *----------------------------------------------------------------*01320000
      * PCB MASKS FOLLOW                                                01330000
        COPY PAUTBPCB.                                                  01340000
```
