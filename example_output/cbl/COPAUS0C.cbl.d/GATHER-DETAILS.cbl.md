```cobol
       GATHER-DETAILS.
      *****************************************************************

           MOVE -1       TO ACCTIDL OF COPAU0AI

           MOVE 0        TO CDEMO-CPVS-PAGE-NUM

           IF WS-ACCT-ID NOT = LOW-VALUES
              PERFORM GATHER-ACCOUNT-DETAILS

              PERFORM INITIALIZE-AUTH-DATA

              IF FOUND-PAUT-SMRY-SEG
                 PERFORM PROCESS-PAGE-FORWARD
              END-IF
           END-IF
           .

```
