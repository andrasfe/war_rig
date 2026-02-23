       COPY CSMSG02Y.

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
       MAIN-PARA.

           SET ERR-FLG-OFF     TO TRUE
           SET SEND-ERASE-YES  TO TRUE

           MOVE SPACES TO WS-MESSAGE
                          ERRMSGO OF COPAU1AO

