      ******************************************************************
      * Program     : CBTRN02C.CBL                                      
      * Application : CardDemo                                          
      * Type        : BATCH COBOL Program                                
      * Function    : Post the records from daily transaction file.     
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.                   
      * All Rights Reserved.                                            
      *                                                                 
      * Licensed under the Apache License, Version 2.0 (the "License"). 
      * You may not use this file except in compliance with the License.
      * You may obtain a copy of the License at                         
      *                                                                 
      *    http://www.apache.org/licenses/LICENSE-2.0                   
      *                                                                 
      * Unless required by applicable law or agreed to in writing,      
      * software distributed under the License is distributed on an     
      * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    
      * either express or implied. See the License for the specific     
      * language governing permissions and limitations under the License
      ******************************************************************
       IDENTIFICATION DIVISION.                                          
       PROGRAM-ID. CBTRN02C.                                            
       AUTHOR. AWS.                                                     
                                                                        
       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
           SELECT DALYTRAN-FILE ASSIGN TO DALYTRAN                      
                  ORGANIZATION IS SEQUENTIAL                            
                  ACCESS MODE  IS SEQUENTIAL                            
                  FILE STATUS  IS DALYTRAN-STATUS.                      
                                                                        
           SELECT TRANSACT-FILE ASSIGN TO TRANFILE                      
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-TRANS-ID                           
                  FILE STATUS  IS TRANFILE-STATUS.                      
                                                                        
           SELECT XREF-FILE ASSIGN TO   XREFFILE                        
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-XREF-CARD-NUM                      
                  FILE STATUS  IS XREFFILE-STATUS.                      
                                                                        
           SELECT DALYREJS-FILE ASSIGN TO DALYREJS                      
                  ORGANIZATION IS SEQUENTIAL                            
                  ACCESS MODE  IS SEQUENTIAL                            
                  FILE STATUS  IS DALYREJS-STATUS.                      
                                                                        
           SELECT ACCOUNT-FILE ASSIGN TO ACCTFILE                       
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-ACCT-ID                            
                  FILE STATUS  IS ACCTFILE-STATUS.                      
                                                                        
           SELECT TCATBAL-FILE ASSIGN TO TCATBALF                       
                  ORGANIZATION IS INDEXED                               
                  ACCESS MODE  IS RANDOM                                
                  RECORD KEY   IS FD-TRAN-CAT-KEY                       
                  FILE STATUS  IS TCATBALF-STATUS.                      
      *                                                                 
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
       FD  DALYTRAN-FILE.                                               
       01  FD-TRAN-RECORD.                                              
           05 FD-TRAN-ID                        PIC X(16).              
           05 FD-CUST-DATA                      PIC X(334).             
                                                                        
       FD  TRANSACT-FILE.                                               
       01  FD-TRANFILE-REC.                                             
           05 FD-TRANS-ID                       PIC X(16).              
           05 FD-ACCT-DATA                      PIC X(334).             
                                                                        
       FD  XREF-FILE.                                                   
       01  FD-XREFFILE-REC.                                             
           05 FD-XREF-CARD-NUM                  PIC X(16).              
           05 FD-XREF-DATA                      PIC X(34).              
                                                                        
       FD  DALYREJS-FILE.                                               
       01  FD-REJS-RECORD.                                              
           05 FD-REJECT-RECORD                  PIC X(350).             
           05 FD-VALIDATION-TRAILER             PIC X(80).              
                                                                        
       FD  ACCOUNT-FILE.                                                
       01  FD-ACCTFILE-REC.                                             
           05 FD-ACCT-ID                        PIC 9(11).              
           05 FD-ACCT-DATA                      PIC X(289).             
                                                                        
       FD  TCATBAL-FILE.                                                
       01  FD-TRAN-CAT-BAL-RECORD.                                      
           05 FD-TRAN-CAT-KEY.                                          
              10 FD-TRANCAT-ACCT-ID             PIC 9(11).              
              10 FD-TRANCAT-TYPE-CD             PIC X(02).              
              10 FD-TRANCAT-CD                  PIC 9(04).              
           05 FD-FD-TRAN-CAT-DATA               PIC X(33).              
                                                                        
       WORKING-STORAGE SECTION.                                         
                                                                        
      *****************************************************************
       COPY CVTRA06Y.                                                   
       01  DALYTRAN-STATUS.                                             
           05  DALYTRAN-STAT1      PIC X.                               
           05  DALYTRAN-STAT2      PIC X.                               
                                                                        
       COPY CVTRA05Y.                                                   
       01  TRANFILE-STATUS.                                             
           05  TRANFILE-STAT1      PIC X.                               
           05  TRANFILE-STAT2      PIC X.                               
                                                                        
       COPY CVACT03Y.                                                   
       01  XREFFILE-STATUS.                                             
           05  XREFFILE-STAT1      PIC X.                               
           05  XREFFILE-STAT2      PIC X.                               
                                                                        
       01  DALYREJS-STATUS.                                             
           05  DALYREJS-STAT1      PIC X.                               
           05  DALYREJS-STAT2      PIC X.                               
                                                                        
       COPY CVACT01Y.                                                   
       01  ACCTFILE-STATUS.                                             
           05  ACCTFILE-STAT1      PIC X.                               
           05  ACCTFILE-STAT2      PIC X.                               
                                                                        
       COPY CVTRA01Y.                                                   
       01  TCATBALF-STATUS.                                             
           05  TCATBALF-STAT1      PIC X.                               
           05  TCATBALF-STAT2      PIC X.                               
                                                                        
       01  IO-STATUS.                                                   
           05  IO-STAT1            PIC X.                               
           05  IO-STAT2            PIC X.                               
       01  TWO-BYTES-BINARY        PIC 9(4) COMP.                       
       01  TWO-BYTES-ALPHA         REDEFINES TWO-BYTES-BINARY.          
           05  TWO-BYTES-LEFT      PIC X.                               
           05  TWO-BYTES-RIGHT     PIC X.                               
       01  IO-STATUS-04.                                                
           05  IO-STATUS-0401      PIC 9   VALUE 0.                     
           05  IO-STATUS-0403      PIC 999 VALUE 0.                     
                                                                        
       01  APPL-RESULT             PIC S9(9)   COMP.                    
           88  APPL-AOK            VALUE 0.                             
           88  APPL-EOF            VALUE 16.                            
                                                                        
       01  END-OF-FILE             PIC X(01)    VALUE 'N'.              
       01  ABCODE                  PIC S9(9) COMP.                      
       01  TIMING                  PIC S9(9) COMP.                      
      * T I M E S T A M P   D B 2  X(26)     EEEE-MM-DD-UU.MM.SS.HH0000 
       01  COBOL-TS.                                                    
           05 COB-YYYY                  PIC X(04).                      
           05 COB-MM                    PIC X(02).                      
           05 COB-DD                    PIC X(02).                      
           05 COB-HH                    PIC X(02).                      
           05 COB-MIN                   PIC X(02).                      
           05 COB-SS                    PIC X(02).                      
           05 COB-MIL                   PIC X(02).                      
           05 COB-REST                  PIC X(05).                      
       01  DB2-FORMAT-TS                PIC X(26).                      
       01  FILLER REDEFINES DB2-FORMAT-TS.                              
           06 DB2-YYYY                  PIC X(004).                     
           06 DB2-STREEP-1              PIC X.                          
           06 DB2-MM                    PIC X(002).                     
           06 DB2-STREEP-2              PIC X.                          
           06 DB2-DD                    PIC X(002).                     
           06 DB2-STREEP-3              PIC X.                          
           06 DB2-HH                    PIC X(002).                     
           06 DB2-DOT-1                 PIC X.                          
           06 DB2-MIN                   PIC X(002).                     
           06 DB2-DOT-2                 PIC X.                          
           06 DB2-SS                    PIC X(002).                     
           06 DB2-DOT-3                 PIC X.                          
           06 DB2-MIL                   PIC 9(002).                     
           06 DB2-REST                  PIC X(04).                      
                                                                        
        01 REJECT-RECORD.                                               
           05 REJECT-TRAN-DATA          PIC X(350).                     
           05 VALIDATION-TRAILER        PIC X(80).                      
                                                                        
        01 WS-VALIDATION-TRAILER.                                       
           05 WS-VALIDATION-FAIL-REASON      PIC 9(04).                 
           05 WS-VALIDATION-FAIL-REASON-DESC PIC X(76).                 
                                                                        
        01 WS-COUNTERS.                                                 
           05 WS-TRANSACTION-COUNT          PIC 9(09) VALUE 0.          
           05 WS-REJECT-COUNT               PIC 9(09) VALUE 0.          
           05 WS-TEMP-BAL                   PIC S9(09)V99.              
                                                                        
        01 WS-FLAGS.                                                    
           05 WS-CREATE-TRANCAT-REC         PIC X(01) VALUE 'N'.        
                                                                        
      *****************************************************************
       PROCEDURE DIVISION.                                              
           DISPLAY 'START OF EXECUTION OF PROGRAM CBTRN02C'.            
           PERFORM 0000-DALYTRAN-OPEN.                                  
           PERFORM 0100-TRANFILE-OPEN.                                  
           PERFORM 0200-XREFFILE-OPEN.                                  
           PERFORM 0300-DALYREJS-OPEN.                                  
           PERFORM 0400-ACCTFILE-OPEN.                                  
           PERFORM 0500-TCATBALF-OPEN.                                  
                                                                        
           PERFORM 1000-PROCESS-TRANSACTIONS                            
               UNTIL END-OF-FILE = 'Y'.                                 
                                                                        
           PERFORM 9000-DALYTRAN-CLOSE.                                 
           PERFORM 9100-TRANFILE-CLOSE.                                 
           PERFORM 9200-XREFFILE-CLOSE.                                 
           PERFORM 9300-DALYREJS-CLOSE.                                 
           PERFORM 9400-ACCTFILE-CLOSE.                                 
           PERFORM 9500-TCATBALF-CLOSE.                                 
           DISPLAY 'TRANSACTIONS PROCESSED :' WS-TRANSACTION-COUNT.     
           DISPLAY 'TRANSACTIONS REJECTED  :' WS-REJECT-COUNT.          
           IF WS-REJECT-COUNT > 0                                       
              MOVE 4 TO RETURN-CODE.                                    
                                                                        
           DISPLAY 'END OF EXECUTION OF PROGRAM CBTRN02C'.              
                                                                        
           GOBACK.                                                      
                                                                        
      *---------------------------------------------------------------*
       1000-PROCESS-TRANSACTIONS.                                       
           PERFORM 1100-DALYTRAN-GET-NEXT.                              
           IF END-OF-FILE = 'N'                                         
               ADD 1 TO WS-TRANSACTION-COUNT                            
               MOVE 0 TO WS-VALIDATION-FAIL-REASON                      
               MOVE SPACES TO WS-VALIDATION-FAIL-REASON-DESC            
               PERFORM 1500-VALIDATE-TRAN                               
               IF WS-VALIDATION-FAIL-REASON = 0                         
                   PERFORM 2000-POST-TRANSACTION                        
               ELSE                                                     
                   ADD 1 TO WS-REJECT-COUNT                             
                   PERFORM 2500-WRITE-REJECT-REC.                       
                                                                        
      *---------------------------------------------------------------*
       1100-DALYTRAN-GET-NEXT.                                          
           READ DALYTRAN-FILE INTO DALYTRAN-RECORD.                     
           IF DALYTRAN-STATUS = '00'                                    
               MOVE 0 TO APPL-RESULT                                    
           ELSE                                                         
               IF DALYTRAN-STATUS = '10'                                
                   MOVE 16 TO APPL-RESULT                               
               ELSE                                                     
                   MOVE 12 TO APPL-RESULT.                              
                                                                        
           IF APPL-AOK                                                  
               NEXT SENTENCE                                            
           ELSE                                                         
               IF APPL-EOF                                              
                   MOVE 'Y' TO END-OF-FILE                              
               ELSE                                                     
                   DISPLAY 'ERROR READING DALYTRAN FILE'                
                   MOVE DALYTRAN-STATUS TO IO-STATUS                    
                   PERFORM 9910-DISPLAY-IO-STATUS                       
                   PERFORM 9999-ABEND-PROGRAM.
