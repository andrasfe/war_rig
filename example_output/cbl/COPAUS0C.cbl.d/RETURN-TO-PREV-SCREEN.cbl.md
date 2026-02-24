```cobol
       RETURN-TO-PREV-SCREEN.
      *****************************************************************

           IF CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES
               MOVE 'COSGN00C' TO CDEMO-TO-PROGRAM
           END-IF
           MOVE WS-CICS-TRANID  TO CDEMO-FROM-TRANID
           MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
           MOVE ZEROS           TO CDEMO-PGM-CONTEXT
           EXEC CICS
               XCTL PROGRAM(CDEMO-TO-PROGRAM)
               COMMAREA(CARDDEMO-COMMAREA)
           END-EXEC.

```
