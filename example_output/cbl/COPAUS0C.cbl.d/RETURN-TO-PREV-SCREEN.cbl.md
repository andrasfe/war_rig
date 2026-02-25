```cobol
                  ' System error while reading CUST file. Resp:'
                  WS-RESP-CD-DIS ' Reas:' WS-REAS-CD-DIS
                  DELIMITED BY SIZE
                  INTO WS-MESSAGE
                  END-STRING
                  MOVE -1       TO ACCTIDL OF COPAU0AI
                  PERFORM SEND-PAULST-SCREEN
           END-EVALUATE
           .

      *****************************************************************
       GET-AUTH-SUMMARY.
      *****************************************************************

           PERFORM SCHEDULE-PSB

           MOVE CDEMO-ACCT-ID                   TO PA-ACCT-ID
      *    MOVE XREF-ACCT-ID                    TO PA-ACCT-ID
           EXEC DLI GU USING PCB(PAUT-PCB-NUM)
               SEGMENT (PAUTSUM0)
               INTO (PENDING-AUTH-SUMMARY)
               WHERE (ACCNTID = PA-ACCT-ID)
           END-EXEC

           MOVE DIBSTAT                          TO IMS-RETURN-CODE
           EVALUATE TRUE
```
