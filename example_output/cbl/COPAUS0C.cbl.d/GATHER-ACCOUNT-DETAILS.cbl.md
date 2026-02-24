```cobol

      *Common Messages
       COPY CSMSG01Y.

      *Abend Variables
       COPY CSMSG02Y.

      *ACCOUNT RECORD LAYOUT
       COPY CVACT01Y.

      *CUSTOMER RECORD LAYOUT
       COPY CVACT02Y.

      *CARD XREF LAYOUT
       COPY CVACT03Y.

      *CUSTOMER LAYOUT
       COPY CVCUS01Y.

      *----------------------------------------------------------------*
      *  IMS SEGMENT LAYOUT
      *----------------------------------------------------------------*

      *- PENDING AUTHORIZATION SUMMARY SEGMENT - ROOT
       01 PENDING-AUTH-SUMMARY.
       COPY CIPAUSMY.

      *- PENDING AUTHORIZATION DETAILS SEGMENT - CHILD
       01 PENDING-AUTH-DETAILS.
       COPY CIPAUDTY.


       COPY DFHAID.
       COPY DFHBMSCA.

       LINKAGE SECTION.
       01  DFHCOMMAREA.
         05  LK-COMMAREA                           PIC X(01)
             OCCURS 1 TO 32767 TIMES DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.
      *****************************************************************
       MAIN-PARA.
      *****************************************************************

           SET ERR-FLG-OFF TO TRUE
           SET AUTHS-NOT-EOF TO TRUE
           SET NEXT-PAGE-NO TO TRUE
           SET SEND-ERASE-YES TO TRUE

           MOVE SPACES TO WS-MESSAGE ERRMSGO OF COPAU0AO

           MOVE -1       TO ACCTIDL OF COPAU0AI

           IF EIBCALEN = 0
               INITIALIZE CARDDEMO-COMMAREA
               MOVE WS-PGM-AUTH-SMRY    TO CDEMO-TO-PROGRAM

               SET CDEMO-PGM-REENTER    TO TRUE
```
