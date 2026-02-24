```cobol
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0004I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0004I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(4)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN SEL0005I OF COPAU0AI NOT = SPACES AND LOW-VALUES
                   MOVE SEL0005I OF COPAU0AI TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE CDEMO-CPVS-AUTH-KEYS(5)
                                             TO CDEMO-CPVS-PAU-SELECTED
                  WHEN OTHER
                   MOVE SPACES   TO CDEMO-CPVS-PAU-SEL-FLG
                   MOVE SPACES   TO CDEMO-CPVS-PAU-SELECTED
                END-EVALUATE
                IF (CDEMO-CPVS-PAU-SEL-FLG NOT = SPACES AND LOW-VALUES)
                   AND
                   (CDEMO-CPVS-PAU-SELECTED NOT = SPACES AND LOW-VALUES)
                   EVALUATE CDEMO-CPVS-PAU-SEL-FLG
                     WHEN 'S'
                     WHEN 's'
                        MOVE WS-PGM-AUTH-DTL  TO CDEMO-TO-PROGRAM
                        MOVE WS-CICS-TRANID   TO CDEMO-FROM-TRANID
                        MOVE WS-PGM-AUTH-SMRY TO CDEMO-FROM-PROGRAM
                        MOVE 0                TO CDEMO-PGM-CONTEXT
                        SET CDEMO-PGM-ENTER   TO TRUE

                        EXEC CICS
                            XCTL PROGRAM(CDEMO-TO-PROGRAM)
                            COMMAREA(CARDDEMO-COMMAREA)
                        END-EXEC
                     WHEN OTHER
                       MOVE
                       'Invalid selection. Valid value is S'
                                              TO WS-MESSAGE
                       MOVE -1                TO ACCTIDL OF COPAU0AI
                   END-EVALUATE
                END-IF

              END-IF
           END-IF

           PERFORM GATHER-DETAILS
           .


      *****************************************************************
       GATHER-DETAILS.
      *****************************************************************

           MOVE -1       TO ACCTIDL OF COPAU0AI
```
