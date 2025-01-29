       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID. KHC031BP.                                            
      *-----------------------------------------------------------------
      * AUTHOR.          SE:YAMAMOTO  PGM:KASUYA.                       
      * DATE-WRITTEN.     2008/01/11.                                   
      * DATE-COMPILED.    TODAY.                                        
      * REMARKS. �X�ʃA�E�g�p�䒠�쐬�q�|�P.                            
      *-----------------------------------------------------------------
      * �e�[�u�� Fetch Loop �e�[�u�� �X�V                               
      * �C������ �Ǘ��m�n(004668) �C�����t(20111020) �C����(ZN)         
      * �C������ �Ǘ��m�n(004722) �C�����t(20111117) �C����(ZN)         
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.                                            
       CONFIGURATION SECTION.                                           
       SPECIAL-NAMES.                                                   
           STDCALL IS DLL                                               
           ARGUMENT-NUMBER IS ARGNUM                                    
           ARGUMENT-VALUE  IS ARGVAL                                    
           .                                                            
       EXTERNAL-PROGRAM SECTION.                                        
           CALL-CONVENTION.                                             
           'KXU001SP' IS DLL                                            
           'KXU002SC' IS DLL                                            
           'KXU009SC' IS DLL                                            
           .                                                            
                                                                        
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
       DATA DIVISION.                                                   
       FILE SECTION.                                                    
                                                                        
      ******************************************************************
       WORKING-STORAGE   SECTION.                                       
                                                                        
      ** ���O���G���A��`                                             
                                                                        
           COPY XAU0101    PREFIXING D-.                                
       01  KXU002SC               PIC  X(008) VALUE 'KXU002SC'.         
       01  KXU009SC               PIC  X(008) VALUE 'KXU009SC'.         
       01  WKPGMID                PIC  X(010) VALUE 'KHC031BP'.         
                                                                        
       01  ABND-CODE SYNC         PIC S9(004) COMP VALUE 16.            
       01  ARGCNT                 PIC  9(002) VALUE  0.                 
       01  ARGDATA                PIC  X(008) VALUE SPACE.              
       01  MAXARG                 PIC  9(001) VALUE  1.                 
                                                                        
           EXEC SQL BEGIN DECLARE SECTION END-EXEC.                     
                                                                        
      ** �z�X�g�ϐ���`                                                 
                                                                        
       01 W-���o����.                                                   
          03  W-���N���o�J�n�N��.                                       
            05  W-���N���o�J�n�N     PIC  X(4).                         
            05  W-���N���o�J�n��     PIC  X(2).                         
          03  W-���N���o�I���N��.                                       
            05  W-���N���o�I���N     PIC  X(4).                         
            05  W-���N���o�I����     PIC  X(2).                         
          03  W-�O�N���o�J�n�N��.                                       
            05  W-�O�N���o�J�n�N     PIC  X(4).                         
            05  W-�O�N���o�J�n��     PIC  X(2).                         
          03  W-�O�N���o�I���N��.                                       
            05  W-�O�N���o�I���N     PIC  X(4).                         
            05  W-�O�N���o�I����     PIC  X(2).                         
          03  W-�S���X�^�C�v       PIC  9(4).                           
       01 WK���o����.                                                   
          03  WK���N���o�J�n�N��     PIC  X(6).                         
          03  WK���N���o�I���N��     PIC  X(6).                         
          03  WK�O�N���o�J�n�N��     PIC  X(6).                         
          03  WK�O�N���o�I���N��     PIC  X(6).                         
          03  WK���N�x             PIC  X(4).                           
          03  WK�O�N�x             PIC  X(4).                           
          03  WK�Ώ۔N���x         PIC  X(6).                           
          03  WK�Ώۂ`�a�敪       PIC  X(1).                           
                                                                        
       01  W-COUNTER.                                                   
           03  SYS040-CNT         PIC  9(011)  VALUE 0.                 
           03  SYS045-CNT         PIC  9(011)  VALUE 0.                 
           03  INSERT-CNT         PIC  9(011)  VALUE 0.                 
           03  UPDATE-CNT         PIC  9(011)  VALUE 0.                 
           03  DELETE-CNT         PIC  9(011)  VALUE 0.                 
           03  ERROR-CNT          PIC  9(011)  VALUE 0.                 
           03  EXIST-CNT          PIC  9(011)  VALUE 0.                 
           03  NOT-EXIST-CNT      PIC  9(011)  VALUE 0.                 
                                                                        
           EXEC SQL END DECLARE SECTION END-EXEC.                       
                                                                        
           EXEC SQL INCLUDE SQLCA            END-EXEC.                  
           EXEC SQL INCLUDE XAU0201.CBL      END-EXEC.                  
           EXEC SQL INCLUDE XAT0201.CBL      END-EXEC.                  
           EXEC SQL INCLUDE XAU0901.CBL      END-EXEC.                  
           EXEC SQL INCLUDE HXU0102.CBL      END-EXEC.                  
                                                                        
      ******************************************************************
       PROCEDURE         DIVISION.                                      
      *-----------------------------------------------------------------
       �又��.                                                          
      *-----------------------------------------------------------------
                                                                        
      ** ��������                                                       
                                                                        
           MOVE  SPACE        TO D-���O���                             
           MOVE  WKPGMID      TO D-�v���O�����h�c                       
           MOVE  'STARTED   ' TO D-�J�E���g���o��                       
           CALL  KXU002SC  USING  D-���O���                            
                                                                        
           MOVE 'HCM501'      TO �W���u�R�[�h                           
           MOVE  1            TO �p�����[�^�ԍ�                         
           MOVE  1            TO �s�ԍ�                                 
                                                                        
      ** ���C��                                                         
                                                                        
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERROR END-EXEC.    
           PERFORM  �c�a�ڑ����擾����.                               
           PERFORM  �c�a�ڑ�����.                                       
           PERFORM  �p�����[�^�擾����.                                 
           PERFORM  �p�����[�^���O�o�͏���.                             
           PERFORM  �䒠�폜����.                                       
           PERFORM  �䒠�ǉ�����.                                       
           PERFORM  �I������.                                           
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       �c�a�ڑ����擾����.                                            
      *-----------------------------------------------------------------
           ACCEPT ARGCNT  FROM ARGNUM                                   
           IF ARGCNT > MAXARG                                           
             THEN                                                       
               MOVE '�����Ɍ�肪����܂�'   TO D-���b�Z�[�W���        
               CALL KXU002SC              USING D-���O���              
               MOVE ABND-CODE                TO RETURN-CODE             
               STOP RUN                                                 
             ELSE                                                       
               IF ARGCNT = 0                                            
                 THEN                                                   
                   MOVE SPACE                TO ARGDATA                 
                 ELSE                                                   
                   ACCEPT ARGDATA FROM ARGVAL                           
                     ON EXCEPTION                                       
                       MOVE '�����Ɍ�肪����܂�' TO D-���b�Z�[�W���  
                       CALL KXU002SC            USING D-���O���        
                       MOVE ABND-CODE              TO RETURN-CODE       
                       STOP RUN                                         
                     NOT ON EXCEPTION                                   
                       CONTINUE                                         
                   END-ACCEPT                                           
               END-IF                                                   
           END-IF                                                       
      *                                                                 
           CALL KXU009SC  USING  BY VALUE      ARGDATA                  
                                 BY REFERENCE  USERNAME                 
                                               PASSWD                   
                                               DB-NAME                  
           IF RETURN-CODE NOT = 0                                       
             THEN                                                       
               STOP RUN                                                 
             ELSE                                                       
               CONTINUE                                                 
           END-IF.                                                      
                                                                        
      *-----------------------------------------------------------------
       �c�a�ڑ�����.                                                    
      *-----------------------------------------------------------------
           EXEC SQL                                                     
             CONNECT :USERNAME IDENTIFIED BY :PASSWD AT :DB-NAME        
           END-EXEC.                                                    
                                                                        
      *-----------------------------------------------------------------
       SQL-ERROR.                                                       
      *-----------------------------------------------------------------
           MOVE  '�r�p�k�G���[' TO D-���b�Z�[�W���.                    
           CALL  KXU002SC  USING  D-���O���.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-���b�Z�[�W���.           
           MOVE SQLERRMC TO D-���b�Z�[�W���(25:100).                   
           CALL KXU002SC USING D-���O���.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       �p�����[�^�擾����.                                              
      *-----------------------------------------------------------------
           EXEC SQL WHENEVER NOT FOUND DO PERFORM SQL-ERRORP END-EXEC.  
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORP END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
             SELECT *                                                   
               INTO :�p�����[�^�t�@�C��                                 
               FROM XAT_PARAM TP                                        
              WHERE TP.�W���u�R�[�h   = :�W���u�R�[�h                   
                AND TP.�p�����[�^�ԍ� = :�p�����[�^�ԍ�                 
                AND TP.�s�ԍ�         = :�s�ԍ�                         
           END-EXEC.                                                    
           MOVE �p�����[�^�t�@�C��  TO �N���w��p�����[�^REC(1:90)      
                                                                        
      *     IF �Ώ۔N���x(5:2) = '01' OR '02' OR '03'                    
      *       THEN                                                       
      *          MOVE �Ώ۔N���x(1:4)   TO W-�S���X�^�C�v                
      *          COMPUTE W-�S���X�^�C�v =  W-�S���X�^�C�v - 1            
      *          MOVE W-�S���X�^�C�v    TO W-���N���o�J�n�N              
      *       ELSE                                                       
      *          MOVE �Ώ۔N���x(1:4)   TO W-���N���o�J�n�N              
      *     END-IF                                                       
      *     MOVE '04'              TO W-���N���o�J�n��                   
           MOVE �Ώ۔N���x(1:4)   TO W-���N���o�J�n�N              
           MOVE '01'              TO W-���N���o�J�n��                   
                                                                        
           MOVE �Ώ۔N���x        TO W-���N���o�I���N��                 
                                                                        
           MOVE W-���N���o�J�n�N  TO W-�S���X�^�C�v                     
           COMPUTE W-�S���X�^�C�v  = W-�S���X�^�C�v - 1                 
           MOVE W-�S���X�^�C�v    TO W-�O�N���o�J�n�N                   
           MOVE W-���N���o�J�n��  TO W-�O�N���o�J�n��                   
                                                                        
           MOVE W-���N���o�I���N  TO W-�S���X�^�C�v                     
           COMPUTE W-�S���X�^�C�v  = W-�S���X�^�C�v - 1                 
           MOVE W-�S���X�^�C�v    TO W-�O�N���o�I���N                   
           MOVE W-���N���o�I����  TO W-�O�N���o�I����                   
                                                                        
           MOVE W-���N���o�J�n�N��  TO WK���N���o�J�n�N��               
           MOVE W-���N���o�I���N��  TO WK���N���o�I���N��               
           MOVE W-�O�N���o�J�n�N��  TO WK�O�N���o�J�n�N��               
           MOVE W-�O�N���o�I���N��  TO WK�O�N���o�I���N��               
           MOVE W-���N���o�J�n�N    TO WK���N�x                         
           MOVE W-�O�N���o�J�n�N    TO WK�O�N�x                         
           MOVE �Ώ۔N���x          TO WK�Ώ۔N���x                     
           MOVE �Ώۂ`�a�敪        TO WK�Ώۂ`�a�敪.                  
                                                                        
      *-----------------------------------------------------------------
       SQL-ERRORP.                                                      
      *-----------------------------------------------------------------
           MOVE  '�r�p�k�G���[ �p�����[�^�擾����' TO D-���b�Z�[�W���. 
           CALL  KXU002SC  USING  D-���O���.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-���b�Z�[�W���.           
           MOVE SQLERRMC TO D-���b�Z�[�W���(25:100).                   
           CALL KXU002SC USING D-���O���.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       �p�����[�^���O�o�͏���.                                          
      *-----------------------------------------------------------------
           MOVE '�̔��V�X�e��  �����p�p�����[�^          '              
                                              TO D-���b�Z�[�W���(1:40) 
           MOVE '�Ώ۔N���x�`�a = @@@@@@*  '  TO D-���b�Z�[�W���(41:27)
           MOVE  �Ώ۔N���x                   TO D-���b�Z�[�W���(58:6) 
           MOVE  �Ώۂ`�a�敪                 TO D-���b�Z�[�W���(64:1) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '���������N��   = @@@@@@   '  TO D-���b�Z�[�W���(41:27)
           MOVE  ���������N��                 TO D-���b�Z�[�W���(58:6) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '������N��   = @@@@@@   '  TO D-���b�Z�[�W���(41:27)
           MOVE  ������N��                 TO D-���b�Z�[�W���(58:6) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '�K�p�J�n�N���� = @@@@@@@@ '  TO D-���b�Z�[�W���(41:27)
           MOVE  �K�p�J�n�N����               TO D-���b�Z�[�W���(58:8) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '�K�p�I���N���� = @@@@@@@@ '  TO D-���b�Z�[�W���(41:27)
           MOVE  �K�p�I���N����               TO D-���b�Z�[�W���(58:8) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '���N���o�J�n�N��=@@@@MM   '  TO D-���b�Z�[�W���(41:27)
           MOVE  WK���N���o�J�n�N��           TO D-���b�Z�[�W���(58:6) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '���N���o�I���N��=@@@@MM   '  TO D-���b�Z�[�W���(41:27)
           MOVE  WK���N���o�I���N��           TO D-���b�Z�[�W���(58:6) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '�O�N���o�J�n�N��=@@@@MM   '  TO D-���b�Z�[�W���(41:27)
           MOVE  WK�O�N���o�J�n�N��           TO D-���b�Z�[�W���(58:6) 
           CALL KXU002SC  USING  D-���O���                             
                                                                        
           MOVE '�O�N���o�I���N��=@@@@MM   '  TO D-���b�Z�[�W���(41:27)
           MOVE  WK�O�N���o�I���N��           TO D-���b�Z�[�W���(58:6) 
           CALL KXU002SC  USING  D-���O���.                            
                                                                        
      *-----------------------------------------------------------------
       �䒠�폜����.                                                    
      *-----------------------------------------------------------------
           EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.               
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORD END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
            DELETE FROM  HCT_OUT_MISE_DFILE_NEW TD                          
              WHERE TD.������N�� <= :WK�Ώ۔N���x                    
           END-EXEC.                                                    
           MOVE  SQLERRD(3)  TO  DELETE-CNT.                            
                                                                        
           MOVE '�䒠�폜�����@����' TO D-���b�Z�[�W���.               
           CALL KXU002SC   USING  D-���O���.                           
                                                                        
      *-----------------------------------------------------------------
       SQL-ERRORD.                                                      
      *-----------------------------------------------------------------
           MOVE  '�r�p�k�G���[ �䒠�폜����' TO D-���b�Z�[�W���.       
           CALL  KXU002SC  USING  D-���O���.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-���b�Z�[�W���.           
           MOVE SQLERRMC TO D-���b�Z�[�W���(25:100).                   
           CALL KXU002SC USING D-���O���.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       �䒠�ǉ�����.                                                    
      *-----------------------------------------------------------------
           EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.               
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORI END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
            INSERT  INTO  HCT_OUT_MISE_DFILE_NEW                            
            SELECT                                                      
               :WK���N�x                                                
              ,:WK�Ώ۔N���x                                            
              ,T.���Ӑ�R�[�h�}�e                                       
              ,T.�u�����h�R�[�h                                         
              , CASE T.�����a�j0                                        
                WHEN 'ZD0' THEN 'XA0'                                   
                WHEN 'ZF0' THEN 'XB0'                                   
                WHEN 'ZE0' THEN 'XC0'                                   
                WHEN 'ZG0' THEN 'XD0'                                   
                ELSE T.�����a�j0                                        
                END                                                     
              ,T.������                                               
              ,T.����N����                                             
              ,T.���N����                                             
              ,T.�����ԑ���                                           
              ,T.����N��������                                         
              ,T.���N��������                                         
              ,T.�`���l���敪                                           
              ,T.�V�`���l���敪                                         
              ,T.�r�q�敪                                               
              ,T.�N���X�R�[�h                                           
              ,T.�ƑԂQ�敪                                             
              ,T.�̎����敪                                             
              ,T.���ߑ��E                                               
              ,T.�������@                                               
              ,T.�x��������`�L�[                                       
              ,T.���������Ώ�                                           
              ,T.�������t�H�[�}�b�g                                     
              ,T.�O�N���x�V�K�敪                                       
              ,T.�O�N�݌v�V�K�敪                                       
              ,T.���N���x�V�K�敪                                       
              ,T.���N�݌v�V�K�敪                                       
              ,T.�O�N���x�V�K�敪����                                   
              ,T.�O�N�݌v�V�K�敪����                                   
              ,T.���N���x�V�K�敪����                                   
              ,T.���N�݌v�V�K�敪����                                   
              ,T.�O�N���x���敪                                       
              ,T.�O�N�݌v���敪                                       
              ,T.���N���x���敪                                       
              ,T.���N�݌v���敪                                       
              ,T.�O�N���x���敪����                                   
              ,T.�O�N�݌v���敪����                                   
              ,T.���N���x���敪����                                   
              ,T.���N�݌v���敪����                                   
              ,NVL(S.�O�N���i01,0) AS �O�N���i01                        
              ,NVL(S.�O�N���i02,0) AS �O�N���i02                        
              ,NVL(S.�O�N���i03,0) AS �O�N���i03                        
              ,NVL(S.�O�N���i04,0) AS �O�N���i04                        
              ,NVL(S.�O�N���i05,0) AS �O�N���i05                        
              ,NVL(S.�O�N���i06,0) AS �O�N���i06                        
              ,NVL(S.�O�N���i07,0) AS �O�N���i07                        
              ,NVL(S.�O�N���i08,0) AS �O�N���i08                        
              ,NVL(S.�O�N���i09,0) AS �O�N���i09                        
              ,NVL(S.�O�N���i10,0) AS �O�N���i10                        
              ,NVL(S.�O�N���i11,0) AS �O�N���i11                        
              ,NVL(S.�O�N���i12,0) AS �O�N���i12                        
              ,NVL(S.�O�N���i���,0) AS �O�N���i���                    
              ,NVL(S.�O�N���i����,0) AS �O�N���i����                    
              ,NVL(S.�O�N���i�݌v,0) AS �O�N���i�݌v                    
              ,NVL(S.�O�N����01,0) AS �O�N����01                        
              ,NVL(S.�O�N����02,0) AS �O�N����02                        
              ,NVL(S.�O�N����03,0) AS �O�N����03                        
              ,NVL(S.�O�N����04,0) AS �O�N����04                        
              ,NVL(S.�O�N����05,0) AS �O�N����05                        
              ,NVL(S.�O�N����06,0) AS �O�N����06                        
              ,NVL(S.�O�N����07,0) AS �O�N����07                        
              ,NVL(S.�O�N����08,0) AS �O�N����08                        
              ,NVL(S.�O�N����09,0) AS �O�N����09                        
              ,NVL(S.�O�N����10,0) AS �O�N����10                        
              ,NVL(S.�O�N����11,0) AS �O�N����11                        
              ,NVL(S.�O�N����12,0) AS �O�N����12                        
              ,NVL(S.�O�N�������,0) AS �O�N�������                    
              ,NVL(S.�O�N��������,0) AS �O�N��������                    
              ,NVL(S.�O�N�����݌v,0) AS �O�N�����݌v                    
              ,NVL(S.�O�N�`��01,0) AS �O�N�`��01                        
              ,NVL(S.�O�N�`��02,0) AS �O�N�`��02                        
              ,NVL(S.�O�N�`��03,0) AS �O�N�`��03                        
              ,NVL(S.�O�N�`��04,0) AS �O�N�`��04                        
              ,NVL(S.�O�N�`��05,0) AS �O�N�`��05                        
              ,NVL(S.�O�N�`��06,0) AS �O�N�`��06                        
              ,NVL(S.�O�N�`��07,0) AS �O�N�`��07                        
              ,NVL(S.�O�N�`��08,0) AS �O�N�`��08                        
              ,NVL(S.�O�N�`��09,0) AS �O�N�`��09                        
              ,NVL(S.�O�N�`��10,0) AS �O�N�`��10                        
              ,NVL(S.�O�N�`��11,0) AS �O�N�`��11                        
              ,NVL(S.�O�N�`��12,0) AS �O�N�`��12                        
              ,NVL(S.�O�N�`�ԏ��,0) AS �O�N�`�ԏ��                    
              ,NVL(S.�O�N�`�ԉ���,0) AS �O�N�`�ԉ���                    
              ,NVL(S.�O�N�`�ԗ݌v,0) AS �O�N�`�ԗ݌v                    
              ,NVL(S.�O�N�a��01,0) AS �O�N�a��01                        
              ,NVL(S.�O�N�a��02,0) AS �O�N�a��02                        
              ,NVL(S.�O�N�a��03,0) AS �O�N�a��03                        
              ,NVL(S.�O�N�a��04,0) AS �O�N�a��04                        
              ,NVL(S.�O�N�a��05,0) AS �O�N�a��05                        
              ,NVL(S.�O�N�a��06,0) AS �O�N�a��06                        
              ,NVL(S.�O�N�a��07,0) AS �O�N�a��07                        
              ,NVL(S.�O�N�a��08,0) AS �O�N�a��08                        
              ,NVL(S.�O�N�a��09,0) AS �O�N�a��09                        
              ,NVL(S.�O�N�a��10,0) AS �O�N�a��10                        
              ,NVL(S.�O�N�a��11,0) AS �O�N�a��11                        
              ,NVL(S.�O�N�a��12,0) AS �O�N�a��12                        
              ,NVL(S.�O�N�a�ԏ��,0) AS �O�N�a�ԏ��                    
              ,NVL(S.�O�N�a�ԉ���,0) AS �O�N�a�ԉ���                    
              ,NVL(S.�O�N�a�ԗ݌v,0) AS �O�N�a�ԗ݌v                    
              ,NVL(S.�O�N���01,0) AS �O�N���01                        
              ,NVL(S.�O�N���02,0) AS �O�N���02                        
              ,NVL(S.�O�N���03,0) AS �O�N���03                        
              ,NVL(S.�O�N���04,0) AS �O�N���04                        
              ,NVL(S.�O�N���05,0) AS �O�N���05                        
              ,NVL(S.�O�N���06,0) AS �O�N���06                        
              ,NVL(S.�O�N���07,0) AS �O�N���07                        
              ,NVL(S.�O�N���08,0) AS �O�N���08                        
              ,NVL(S.�O�N���09,0) AS �O�N���09                        
              ,NVL(S.�O�N���10,0) AS �O�N���10                        
              ,NVL(S.�O�N���11,0) AS �O�N���11                        
              ,NVL(S.�O�N���12,0) AS �O�N���12                        
              ,NVL(S.�O�N��ԏ��,0) AS �O�N��ԏ��                    
              ,NVL(S.�O�N��ԉ���,0) AS �O�N��ԉ���                    
              ,NVL(S.�O�N��ԗ݌v,0) AS �O�N��ԗ݌v                    
              ,NVL(S.�O�N������01,0) AS �O�N������01                    
              ,NVL(S.�O�N������02,0) AS �O�N������02                    
              ,NVL(S.�O�N������03,0) AS �O�N������03                    
              ,NVL(S.�O�N������04,0) AS �O�N������04                    
              ,NVL(S.�O�N������05,0) AS �O�N������05                    
              ,NVL(S.�O�N������06,0) AS �O�N������06                    
              ,NVL(S.�O�N������07,0) AS �O�N������07                    
              ,NVL(S.�O�N������08,0) AS �O�N������08                    
              ,NVL(S.�O�N������09,0) AS �O�N������09                    
              ,NVL(S.�O�N������10,0) AS �O�N������10                    
              ,NVL(S.�O�N������11,0) AS �O�N������11                    
              ,NVL(S.�O�N������12,0) AS �O�N������12                    
              ,NVL(S.�O�N���������,0) AS �O�N���������                
              ,NVL(S.�O�N����������,0) AS �O�N����������                
              ,NVL(S.�O�N�������݌v,0) AS �O�N�������݌v                
              ,NVL(S.�O�N�ӔC�z01,0) AS �O�N�ӔC�z01                    
              ,NVL(S.�O�N�ӔC�z02,0) AS �O�N�ӔC�z02                    
              ,NVL(S.�O�N�ӔC�z03,0) AS �O�N�ӔC�z03                    
              ,NVL(S.�O�N�ӔC�z04,0) AS �O�N�ӔC�z04                    
              ,NVL(S.�O�N�ӔC�z05,0) AS �O�N�ӔC�z05                    
              ,NVL(S.�O�N�ӔC�z06,0) AS �O�N�ӔC�z06                    
              ,NVL(S.�O�N�ӔC�z07,0) AS �O�N�ӔC�z07                    
              ,NVL(S.�O�N�ӔC�z08,0) AS �O�N�ӔC�z08                    
              ,NVL(S.�O�N�ӔC�z09,0) AS �O�N�ӔC�z09                    
              ,NVL(S.�O�N�ӔC�z10,0) AS �O�N�ӔC�z10                    
              ,NVL(S.�O�N�ӔC�z11,0) AS �O�N�ӔC�z11                    
              ,NVL(S.�O�N�ӔC�z12,0) AS �O�N�ӔC�z12                    
              ,NVL(S.�O�N�ӔC�z���,0) AS �O�N�ӔC�z���                
              ,NVL(S.�O�N�ӔC�z����,0) AS �O�N�ӔC�z����                
              ,NVL(S.�O�N�ӔC�z�݌v,0) AS �O�N�ӔC�z�݌v                
              ,NVL(S.���N���i01,0) AS ���N���i01                        
              ,NVL(S.���N���i02,0) AS ���N���i02                        
              ,NVL(S.���N���i03,0) AS ���N���i03                        
              ,NVL(S.���N���i04,0) AS ���N���i04                        
              ,NVL(S.���N���i05,0) AS ���N���i05                        
              ,NVL(S.���N���i06,0) AS ���N���i06                        
              ,NVL(S.���N���i07,0) AS ���N���i07                        
              ,NVL(S.���N���i08,0) AS ���N���i08                        
              ,NVL(S.���N���i09,0) AS ���N���i09                        
              ,NVL(S.���N���i10,0) AS ���N���i10                        
              ,NVL(S.���N���i11,0) AS ���N���i11                        
              ,NVL(S.���N���i12,0) AS ���N���i12                        
              ,NVL(S.���N���i���,0) AS ���N���i���                    
              ,NVL(S.���N���i����,0) AS ���N���i����                    
              ,NVL(S.���N���i�݌v,0) AS ���N���i�݌v                    
              ,NVL(S.���N����01,0) AS ���N����01                        
              ,NVL(S.���N����02,0) AS ���N����02                        
              ,NVL(S.���N����03,0) AS ���N����03                        
              ,NVL(S.���N����04,0) AS ���N����04                        
              ,NVL(S.���N����05,0) AS ���N����05                        
              ,NVL(S.���N����06,0) AS ���N����06                        
              ,NVL(S.���N����07,0) AS ���N����07                        
              ,NVL(S.���N����08,0) AS ���N����08                        
              ,NVL(S.���N����09,0) AS ���N����09                        
              ,NVL(S.���N����10,0) AS ���N����10                        
              ,NVL(S.���N����11,0) AS ���N����11                        
              ,NVL(S.���N����12,0) AS ���N����12                        
              ,NVL(S.���N�������,0) AS ���N�������                    
              ,NVL(S.���N��������,0) AS ���N��������                    
              ,NVL(S.���N�����݌v,0) AS ���N�����݌v                    
              ,NVL(S.���N�`��01,0) AS ���N�`��01                        
              ,NVL(S.���N�`��02,0) AS ���N�`��02                        
              ,NVL(S.���N�`��03,0) AS ���N�`��03                        
              ,NVL(S.���N�`��04,0) AS ���N�`��04                        
              ,NVL(S.���N�`��05,0) AS ���N�`��05                        
              ,NVL(S.���N�`��06,0) AS ���N�`��06                        
              ,NVL(S.���N�`��07,0) AS ���N�`��07                        
              ,NVL(S.���N�`��08,0) AS ���N�`��08                        
              ,NVL(S.���N�`��09,0) AS ���N�`��09                        
              ,NVL(S.���N�`��10,0) AS ���N�`��10                        
              ,NVL(S.���N�`��11,0) AS ���N�`��11                        
              ,NVL(S.���N�`��12,0) AS ���N�`��12                        
              ,NVL(S.���N�`�ԏ��,0) AS ���N�`�ԏ��                    
              ,NVL(S.���N�`�ԉ���,0) AS ���N�`�ԉ���                    
              ,NVL(S.���N�`�ԗ݌v,0) AS ���N�`�ԗ݌v                    
              ,NVL(S.���N�a��01,0) AS ���N�a��01                        
              ,NVL(S.���N�a��02,0) AS ���N�a��02                        
              ,NVL(S.���N�a��03,0) AS ���N�a��03                        
              ,NVL(S.���N�a��04,0) AS ���N�a��04                        
              ,NVL(S.���N�a��05,0) AS ���N�a��05                        
              ,NVL(S.���N�a��06,0) AS ���N�a��06                        
              ,NVL(S.���N�a��07,0) AS ���N�a��07                        
              ,NVL(S.���N�a��08,0) AS ���N�a��08                        
              ,NVL(S.���N�a��09,0) AS ���N�a��09                        
              ,NVL(S.���N�a��10,0) AS ���N�a��10                        
              ,NVL(S.���N�a��11,0) AS ���N�a��11                        
              ,NVL(S.���N�a��12,0) AS ���N�a��12                        
              ,NVL(S.���N�a�ԏ��,0) AS ���N�a�ԏ��                    
              ,NVL(S.���N�a�ԉ���,0) AS ���N�a�ԉ���                    
              ,NVL(S.���N�a�ԗ݌v,0) AS ���N�a�ԗ݌v                    
              ,NVL(S.���N���01,0) AS ���N���01                        
              ,NVL(S.���N���02,0) AS ���N���02                        
              ,NVL(S.���N���03,0) AS ���N���03                        
              ,NVL(S.���N���04,0) AS ���N���04                        
              ,NVL(S.���N���05,0) AS ���N���05                        
              ,NVL(S.���N���06,0) AS ���N���06                        
              ,NVL(S.���N���07,0) AS ���N���07                        
              ,NVL(S.���N���08,0) AS ���N���08                        
              ,NVL(S.���N���09,0) AS ���N���09                        
              ,NVL(S.���N���10,0) AS ���N���10                        
              ,NVL(S.���N���11,0) AS ���N���11                        
              ,NVL(S.���N���12,0) AS ���N���12                        
              ,NVL(S.���N��ԏ��,0) AS ���N��ԏ��                    
              ,NVL(S.���N��ԉ���,0) AS ���N��ԉ���                    
              ,NVL(S.���N��ԗ݌v,0) AS ���N��ԗ݌v                    
              ,NVL(S.���N������01,0) AS ���N������01                    
              ,NVL(S.���N������02,0) AS ���N������02                    
              ,NVL(S.���N������03,0) AS ���N������03                    
              ,NVL(S.���N������04,0) AS ���N������04                    
              ,NVL(S.���N������05,0) AS ���N������05                    
              ,NVL(S.���N������06,0) AS ���N������06                    
              ,NVL(S.���N������07,0) AS ���N������07                    
              ,NVL(S.���N������08,0) AS ���N������08                    
              ,NVL(S.���N������09,0) AS ���N������09                    
              ,NVL(S.���N������10,0) AS ���N������10                   
              ,NVL(S.���N������11,0) AS ���N������11                    
              ,NVL(S.���N������12,0) AS ���N������12                    
              ,NVL(S.���N���������,0) AS ���N���������                
              ,NVL(S.���N����������,0) AS ���N����������                
              ,NVL(S.���N�������݌v,0) AS ���N�������݌v                
              ,NVL(S.���N�ӔC�z01,0) AS ���N�ӔC�z01                    
              ,NVL(S.���N�ӔC�z02,0) AS ���N�ӔC�z02                    
              ,NVL(S.���N�ӔC�z03,0) AS ���N�ӔC�z03                    
              ,NVL(S.���N�ӔC�z04,0) AS ���N�ӔC�z04                    
              ,NVL(S.���N�ӔC�z05,0) AS ���N�ӔC�z05                    
              ,NVL(S.���N�ӔC�z06,0) AS ���N�ӔC�z06                    
              ,NVL(S.���N�ӔC�z07,0) AS ���N�ӔC�z07                    
              ,NVL(S.���N�ӔC�z08,0) AS ���N�ӔC�z08                    
              ,NVL(S.���N�ӔC�z09,0) AS ���N�ӔC�z09                    
              ,NVL(S.���N�ӔC�z10,0) AS ���N�ӔC�z10                    
              ,NVL(S.���N�ӔC�z11,0) AS ���N�ӔC�z11                    
              ,NVL(S.���N�ӔC�z12,0) AS ���N�ӔC�z12                    
              ,NVL(S.���N�ӔC�z���,0) AS ���N�ӔC�z���                
              ,NVL(S.���N�ӔC�z����,0) AS ���N�ӔC�z����                
              ,NVL(S.���N�ӔC�z�݌v,0) AS ���N�ӔC�z�݌v                
             FROM                                                       
              (                                                         
              SELECT                                                    
                 T1.���Ӑ�R�[�h�}�e                                    
                ,T1.�����a�j||'0' AS �����a�j0                          
                ,T2.�u�����h�R�[�h                                      
                ,T2.������                                            
                ,T2.����N����                                          
                ,T2.���N����                                          
                ,T1.�����ԑ���                                        
                ,T1.����N��������                                      
                ,T1.���N��������                                      
                ,T1.�`���l���敪                                        
                ,T1.�V�`���l���敪                                      
                ,T1.�r�q�敪                                            
                ,T1.�N���X�R�[�h                                        
                ,T1.�ƑԂQ�敪                                          
                ,T1.�̎����敪                                          
                ,T1.���ߑ��E                                            
                ,T3.�������@                                            
                ,T3.�x��������`�L�[                                    
                ,T3.���������Ώ�                                        
                ,T3.�������t�H�[�}�b�g                                  
                                                                        
                ,CASE WHEN SUBSTR(T2.����N����,1,6) =                  
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x�V�K�敪         
                ,CASE WHEN SUBSTR(T2.����N����,1,6) BETWEEN            
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v�V�K�敪         
                ,CASE WHEN SUBSTR(T2.����N����,1,6) =                  
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x�V�K�敪         
                ,CASE WHEN SUBSTR(T2.����N����,1,6) BETWEEN            
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v�V�K�敪         
                                                                        
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) =              
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x�V�K�敪����     
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) BETWEEN        
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v�V�K�敪����     
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) =              
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x�V�K�敪����     
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) BETWEEN        
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v�V�K�敪����     
                                                                        
                ,CASE WHEN SUBSTR(T2.���N����,1,6) =                  
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x���敪         
                ,CASE WHEN SUBSTR(T2.���N����,1,6) BETWEEN            
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v���敪         
                ,CASE WHEN SUBSTR(T2.���N����,1,6) =                  
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x���敪         
                ,CASE WHEN SUBSTR(T2.���N����,1,6) BETWEEN            
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v���敪         
                                                                        
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) =              
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x���敪����     
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) BETWEEN        
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v���敪����     
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) =              
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x���敪����     
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) BETWEEN        
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v���敪����     
               FROM                                                     
                  HAT_TOKUI_KIHON  T1                                   
                , HAT_TOKUI_BRAND  T2                                   
                , HAT_TOKUI_SYOSAI T3                                   
               WHERE                                                    
                     T1.�Ώ۔N�� = :WK�Ώ۔N���x                        
                 AND T1.�Ώۂ`�a�敪 = :WK�Ώۂ`�a�敪                  
                 AND T1.���Ӑ�R�[�h = T1.���Ӑ�R�[�h�}�e              
                 AND T2.���Ӑ�R�[�h = T2.���Ӑ�R�[�h�}�e              
                 AND T3.���Ӑ�R�[�h = T3.���Ӑ�R�[�h�}�e           
                 AND T2.�u�����h�R�[�h NOT IN ('02', '20', '21', '23',
                                               '13', '66','30',
                                               '80', '81', '82')        
      *           AND T2.������ IN ('0', '1', '2', '3')                 
                 AND T1.���Ӑ�R�[�h = T2.���Ӑ�R�[�h                  
                 AND T1.�Ώ۔N�� = T2.�Ώ۔N��                          
                 AND T1.�Ώۂ`�a�敪 = T2.�Ώۂ`�a�敪                  
                 AND T1.���Ӑ�R�[�h = T3.���Ӑ�R�[�h                  
                 AND T1.�Ώ۔N�� = T3.�Ώ۔N��                          
                 AND T1.�Ώۂ`�a�敪 = T3.�Ώۂ`�a�敪                  
                 AND SUBSTR(T1.�x�X�����R�[�h,1,1) <> 'X'
              ) T                                                       
             ,(                                                         
              SELECT                                                    
                 S1.���Ӑ�R�[�h�}�e                                    
                ,NVL(D1.�u�����h�ϊ��R�[�h,S1.�u�����h�R�[�h)           
                  AS �u�����h�R�[�h                                     
       /* �O�N���i */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i01                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i02                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i03                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i04                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i05                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i06                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i07                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i08                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i09                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                   THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END) 
                  AS �O�N���i10                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i11                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i12                                         
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i���                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i����                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i�݌v                                       
                                                                        
       /* �O�N���� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����01          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����02          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����03          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����04          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����05          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����06          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����07          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����08          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����09          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����10          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����11          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�����i���z ELSE 0 END) AS �O�N�������        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS �O�N��������        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS �O�N�����݌v        
                                                                        
       /* �O�N�`�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��01          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��02          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��03          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��04          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��05          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��06          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��07          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��08          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��09          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��10          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��11          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`�ԗ݌v        
                                                                        
       /* �O�N�a�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��01          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��02          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��03          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��04          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��05          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��06          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��07          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��08          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��09          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��10          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��11          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a�ԗ݌v        
                                                                        
       /* �O�N��� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���01        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���02        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���03        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���04        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���05        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���06        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���07        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���08        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���09        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���10        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���11        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N��ԏ��      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N��ԉ���      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N��ԗ݌v      
                                                                        
       /* �O�N������ */                                                 
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������01        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������02        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������03        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������04        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������05        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������06        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������07        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������08        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������09        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.���������z ELSE 0 END) AS �O�N������10        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.���������z ELSE 0 END) AS �O�N������11       
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.���������z ELSE 0 END) AS �O�N������12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.���������z ELSE 0 END) AS �O�N���������      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS �O�N����������      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS �O�N�������݌v      
                                                                        
       /* �O�N�ӔC�z */                                                 
                ,'0' AS �O�N�ӔC�z01                                    
                ,'0' AS �O�N�ӔC�z02                                    
                ,'0' AS �O�N�ӔC�z03                                    
                ,'0' AS �O�N�ӔC�z04                                    
                ,'0' AS �O�N�ӔC�z05                                    
                ,'0' AS �O�N�ӔC�z06                                    
                ,'0' AS �O�N�ӔC�z07                                    
                ,'0' AS �O�N�ӔC�z08                                    
                ,'0' AS �O�N�ӔC�z09                                    
                ,'0' AS �O�N�ӔC�z10                                    
                ,'0' AS �O�N�ӔC�z11                                    
                ,'0' AS �O�N�ӔC�z12                                    
                ,'0' AS �O�N�ӔC�z���                                  
                ,'0' AS �O�N�ӔC�z����                                  
                ,'0' AS �O�N�ӔC�z�݌v                                  
                                                                        
       /* ���N���i */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i01                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i02                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i03                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i04                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i05                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i06                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i07                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i08                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i09                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i10                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i11                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i12                                         
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i���                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i����                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i�݌v                                       
                                                                        
       /* ���N���� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����01          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����02          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����03          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����04          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����05          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����06          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����07          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����08          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����09          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�����i���z ELSE 0 END) AS ���N����10          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�����i���z ELSE 0 END) AS ���N����11          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�����i���z ELSE 0 END) AS ���N����12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�����i���z ELSE 0 END) AS ���N�������        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS ���N��������        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS ���N�����݌v        
                                                                        
       /* ���N�`�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��01          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��02          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��03          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��04          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��05          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��06          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��07          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��08          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��09          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��10          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��11          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`�ԗ݌v        
                                                                        
       /* ���N�a�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��01          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��02          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��03          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��04          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��05          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��06          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��07          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��08          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��09          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��10          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��11          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a�ԗ݌v        
                                                                        
       /* ���N��� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���01        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���02        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���03        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���04        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���05        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���06        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���07        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���08        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���09        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���10        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���11        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N��ԏ��      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N��ԉ���      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N��ԗ݌v      
                                                                        
       /* ���N������ */                                                 
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.���������z ELSE 0 END) AS ���N������01        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.���������z ELSE 0 END) AS ���N������02        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.���������z ELSE 0 END) AS ���N������03        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.���������z ELSE 0 END) AS ���N������04        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.���������z ELSE 0 END) AS ���N������05        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.���������z ELSE 0 END) AS ���N������06        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.���������z ELSE 0 END) AS ���N������07        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.���������z ELSE 0 END) AS ���N������08        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.���������z ELSE 0 END) AS ���N������09        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.���������z ELSE 0 END) AS ���N������10        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.���������z ELSE 0 END) AS ���N������11        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.���������z ELSE 0 END) AS ���N������12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.���������z ELSE 0 END) AS ���N���������      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS ���N����������      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS ���N�������݌v      
                                                                        
       /* ���N�ӔC�z */                                                 
                ,'0' AS ���N�ӔC�z01                                    
                ,'0' AS ���N�ӔC�z02                                    
                ,'0' AS ���N�ӔC�z03                                    
                ,'0' AS ���N�ӔC�z04                                    
                ,'0' AS ���N�ӔC�z05                                    
                ,'0' AS ���N�ӔC�z06                                    
                ,'0' AS ���N�ӔC�z07                                    
                ,'0' AS ���N�ӔC�z08                                    
                ,'0' AS ���N�ӔC�z09                                    
                ,'0' AS ���N�ӔC�z10                                    
                ,'0' AS ���N�ӔC�z11                                    
                ,'0' AS ���N�ӔC�z12                                    
                ,'0' AS ���N�ӔC�z���                                  
                ,'0' AS ���N�ӔC�z����                                  
                ,'0' AS ���N�ӔC�z�݌v                                  
                                                                        
               FROM                                                     
                  HCT_SALES_PERFORMANCE  S1                             
                ,(SELECT '02' AS �u�����h�R�[�h                         
                        ,'01' AS �u�����h�ϊ��R�[�h FROM dual           
                  UNION ALL                                             
                  SELECT '20','19' FROM dual                            
                  UNION ALL                                             
                  SELECT '21','10' FROM dual                            
       /* 2011.10.20 ADD �ْ߰ǉ� */   
                  UNION ALL                                             
                  SELECT '66','65' FROM dual                            
       /* 2011.11.17 ADD CVS50�ǉ� */   
                  UNION ALL                                             
                  SELECT '30','12' FROM dual                            
                  UNION ALL                                             
                  SELECT '23','22' FROM dual                            
                 ) D1                                                   
               WHERE                                                    
                     S1.�u�����h�R�[�h = D1.�u�����h�R�[�h (+)          
                 AND S1.�u�����h�R�[�h NOT IN ('90', '91', '92')        
                 AND (  (   S1.������N�� >= :WK�O�N���o�J�n�N��      
                        AND S1.������N�� <= :WK�O�N���o�I���N��)     
                     OR (   S1.������N�� >= :WK���N���o�J�n�N��      
                        AND S1.������N�� <= :WK���N���o�I���N��) )   
               GROUP BY                                                 
                     S1.���Ӑ�R�[�h�}�e                                
                    ,NVL(D1.�u�����h�ϊ��R�[�h,S1.�u�����h�R�[�h)       
              ) S                                                       
                                                                        
             WHERE                                                      
                   T.���Ӑ�R�[�h�}�e = S.���Ӑ�R�[�h�}�e (+)          
               AND T.�u�����h�R�[�h = S.�u�����h�R�[�h (+)              
                                                                        
           END-EXEC.                                                    
           MOVE  SQLERRD(3)  TO  INSERT-CNT.                            
                                                                        
           MOVE 'INSERT   ='   TO D-�J�E���g���o��                      
           MOVE INSERT-CNT     TO D-�J�E���g                            
           CALL KXU002SC    USING D-���O���                            

           MOVE '�䒠�ǉ������P����' TO D-���b�Z�[�W���.               
           CALL KXU002SC   USING  D-���O���.                           
                                                                        
           EXEC SQL WHENEVER NOT FOUND CONTINUE END-EXEC.               
           EXEC SQL WHENEVER SQLERROR DO PERFORM SQL-ERRORI END-EXEC.   
           EXEC SQL AT :DB-NAME                                         
            INSERT  INTO  HCT_OUT_MISE_DFILE_NEW                        
            SELECT                                                      
               :WK���N�x                                                
              ,:WK�Ώ۔N���x                                            
              ,T.���Ӑ�R�[�h�}�e                                       
              ,T.�u�����h�R�[�h                                         
              ,T.�����a�j0                                              
              ,T.������                                               
              ,T.����N����                                             
              ,T.���N����                                             
              ,T.�����ԑ���                                           
              ,T.����N��������                                         
              ,T.���N��������                                         
              ,T.�`���l���敪                                           
              ,T.�V�`���l���敪                                         
              ,T.�r�q�敪                                               
              ,T.�N���X�R�[�h                                           
              ,T.�ƑԂQ�敪                                             
              ,T.�̎����敪                                             
              ,T.���ߑ��E                                               
              ,T.�������@                                               
              ,T.�x��������`�L�[                                       
              ,T.���������Ώ�                                           
              ,T.�������t�H�[�}�b�g                                     
              ,T.�O�N���x�V�K�敪                                       
              ,T.�O�N�݌v�V�K�敪                                       
              ,T.���N���x�V�K�敪                                       
              ,T.���N�݌v�V�K�敪                                       
              ,T.�O�N���x�V�K�敪����                                   
              ,T.�O�N�݌v�V�K�敪����                                   
              ,T.���N���x�V�K�敪����                                   
              ,T.���N�݌v�V�K�敪����                                   
              ,T.�O�N���x���敪                                       
              ,T.�O�N�݌v���敪                                       
              ,T.���N���x���敪                                       
              ,T.���N�݌v���敪                                       
              ,T.�O�N���x���敪����                                   
              ,T.�O�N�݌v���敪����                                   
              ,T.���N���x���敪����                                   
              ,T.���N�݌v���敪����                                   
              ,NVL(S.�O�N���i01,0) AS �O�N���i01                        
              ,NVL(S.�O�N���i02,0) AS �O�N���i02                        
              ,NVL(S.�O�N���i03,0) AS �O�N���i03                        
              ,NVL(S.�O�N���i04,0) AS �O�N���i04                        
              ,NVL(S.�O�N���i05,0) AS �O�N���i05                        
              ,NVL(S.�O�N���i06,0) AS �O�N���i06                        
              ,NVL(S.�O�N���i07,0) AS �O�N���i07                        
              ,NVL(S.�O�N���i08,0) AS �O�N���i08                        
              ,NVL(S.�O�N���i09,0) AS �O�N���i09                        
              ,NVL(S.�O�N���i10,0) AS �O�N���i10                        
              ,NVL(S.�O�N���i11,0) AS �O�N���i11                        
              ,NVL(S.�O�N���i12,0) AS �O�N���i12                        
              ,NVL(S.�O�N���i���,0) AS �O�N���i���                    
              ,NVL(S.�O�N���i����,0) AS �O�N���i����                    
              ,NVL(S.�O�N���i�݌v,0) AS �O�N���i�݌v                    
              ,NVL(S.�O�N����01,0) AS �O�N����01                        
              ,NVL(S.�O�N����02,0) AS �O�N����02                        
              ,NVL(S.�O�N����03,0) AS �O�N����03                        
              ,NVL(S.�O�N����04,0) AS �O�N����04                        
              ,NVL(S.�O�N����05,0) AS �O�N����05                        
              ,NVL(S.�O�N����06,0) AS �O�N����06                        
              ,NVL(S.�O�N����07,0) AS �O�N����07                        
              ,NVL(S.�O�N����08,0) AS �O�N����08                        
              ,NVL(S.�O�N����09,0) AS �O�N����09                        
              ,NVL(S.�O�N����10,0) AS �O�N����10                        
              ,NVL(S.�O�N����11,0) AS �O�N����11                        
              ,NVL(S.�O�N����12,0) AS �O�N����12                        
              ,NVL(S.�O�N�������,0) AS �O�N�������                    
              ,NVL(S.�O�N��������,0) AS �O�N��������                    
              ,NVL(S.�O�N�����݌v,0) AS �O�N�����݌v                    
              ,NVL(S.�O�N�`��01,0) AS �O�N�`��01                        
              ,NVL(S.�O�N�`��02,0) AS �O�N�`��02                        
              ,NVL(S.�O�N�`��03,0) AS �O�N�`��03                        
              ,NVL(S.�O�N�`��04,0) AS �O�N�`��04                        
              ,NVL(S.�O�N�`��05,0) AS �O�N�`��05                        
              ,NVL(S.�O�N�`��06,0) AS �O�N�`��06                        
              ,NVL(S.�O�N�`��07,0) AS �O�N�`��07                        
              ,NVL(S.�O�N�`��08,0) AS �O�N�`��08                        
              ,NVL(S.�O�N�`��09,0) AS �O�N�`��09                        
              ,NVL(S.�O�N�`��10,0) AS �O�N�`��10                        
              ,NVL(S.�O�N�`��11,0) AS �O�N�`��11                        
              ,NVL(S.�O�N�`��12,0) AS �O�N�`��12                        
              ,NVL(S.�O�N�`�ԏ��,0) AS �O�N�`�ԏ��                    
              ,NVL(S.�O�N�`�ԉ���,0) AS �O�N�`�ԉ���                    
              ,NVL(S.�O�N�`�ԗ݌v,0) AS �O�N�`�ԗ݌v                    
              ,NVL(S.�O�N�a��01,0) AS �O�N�a��01                        
              ,NVL(S.�O�N�a��02,0) AS �O�N�a��02                        
              ,NVL(S.�O�N�a��03,0) AS �O�N�a��03                        
              ,NVL(S.�O�N�a��04,0) AS �O�N�a��04                        
              ,NVL(S.�O�N�a��05,0) AS �O�N�a��05                        
              ,NVL(S.�O�N�a��06,0) AS �O�N�a��06                        
              ,NVL(S.�O�N�a��07,0) AS �O�N�a��07                        
              ,NVL(S.�O�N�a��08,0) AS �O�N�a��08                        
              ,NVL(S.�O�N�a��09,0) AS �O�N�a��09                        
              ,NVL(S.�O�N�a��10,0) AS �O�N�a��10                        
              ,NVL(S.�O�N�a��11,0) AS �O�N�a��11                        
              ,NVL(S.�O�N�a��12,0) AS �O�N�a��12                        
              ,NVL(S.�O�N�a�ԏ��,0) AS �O�N�a�ԏ��                    
              ,NVL(S.�O�N�a�ԉ���,0) AS �O�N�a�ԉ���                    
              ,NVL(S.�O�N�a�ԗ݌v,0) AS �O�N�a�ԗ݌v                    
              ,NVL(S.�O�N���01,0) AS �O�N���01                        
              ,NVL(S.�O�N���02,0) AS �O�N���02                        
              ,NVL(S.�O�N���03,0) AS �O�N���03                        
              ,NVL(S.�O�N���04,0) AS �O�N���04                        
              ,NVL(S.�O�N���05,0) AS �O�N���05                        
              ,NVL(S.�O�N���06,0) AS �O�N���06                        
              ,NVL(S.�O�N���07,0) AS �O�N���07                        
              ,NVL(S.�O�N���08,0) AS �O�N���08                        
              ,NVL(S.�O�N���09,0) AS �O�N���09                        
              ,NVL(S.�O�N���10,0) AS �O�N���10                        
              ,NVL(S.�O�N���11,0) AS �O�N���11                        
              ,NVL(S.�O�N���12,0) AS �O�N���12                        
              ,NVL(S.�O�N��ԏ��,0) AS �O�N��ԏ��                    
              ,NVL(S.�O�N��ԉ���,0) AS �O�N��ԉ���                    
              ,NVL(S.�O�N��ԗ݌v,0) AS �O�N��ԗ݌v                    
              ,NVL(S.�O�N������01,0) AS �O�N������01                    
              ,NVL(S.�O�N������02,0) AS �O�N������02                    
              ,NVL(S.�O�N������03,0) AS �O�N������03                    
              ,NVL(S.�O�N������04,0) AS �O�N������04                    
              ,NVL(S.�O�N������05,0) AS �O�N������05                    
              ,NVL(S.�O�N������06,0) AS �O�N������06                    
              ,NVL(S.�O�N������07,0) AS �O�N������07                    
              ,NVL(S.�O�N������08,0) AS �O�N������08                    
              ,NVL(S.�O�N������09,0) AS �O�N������09                    
              ,NVL(S.�O�N������10,0) AS �O�N������10                    
              ,NVL(S.�O�N������11,0) AS �O�N������11                    
              ,NVL(S.�O�N������12,0) AS �O�N������12                    
              ,NVL(S.�O�N���������,0) AS �O�N���������                
              ,NVL(S.�O�N����������,0) AS �O�N����������                
              ,NVL(S.�O�N�������݌v,0) AS �O�N�������݌v                
              ,NVL(S.�O�N�ӔC�z01,0) AS �O�N�ӔC�z01                    
              ,NVL(S.�O�N�ӔC�z02,0) AS �O�N�ӔC�z02                    
              ,NVL(S.�O�N�ӔC�z03,0) AS �O�N�ӔC�z03                    
              ,NVL(S.�O�N�ӔC�z04,0) AS �O�N�ӔC�z04                    
              ,NVL(S.�O�N�ӔC�z05,0) AS �O�N�ӔC�z05                    
              ,NVL(S.�O�N�ӔC�z06,0) AS �O�N�ӔC�z06                    
              ,NVL(S.�O�N�ӔC�z07,0) AS �O�N�ӔC�z07                    
              ,NVL(S.�O�N�ӔC�z08,0) AS �O�N�ӔC�z08                    
              ,NVL(S.�O�N�ӔC�z09,0) AS �O�N�ӔC�z09                    
              ,NVL(S.�O�N�ӔC�z10,0) AS �O�N�ӔC�z10                    
              ,NVL(S.�O�N�ӔC�z11,0) AS �O�N�ӔC�z11                    
              ,NVL(S.�O�N�ӔC�z12,0) AS �O�N�ӔC�z12                    
              ,NVL(S.�O�N�ӔC�z���,0) AS �O�N�ӔC�z���                
              ,NVL(S.�O�N�ӔC�z����,0) AS �O�N�ӔC�z����                
              ,NVL(S.�O�N�ӔC�z�݌v,0) AS �O�N�ӔC�z�݌v                
              ,NVL(S.���N���i01,0) AS ���N���i01                        
              ,NVL(S.���N���i02,0) AS ���N���i02                        
              ,NVL(S.���N���i03,0) AS ���N���i03                        
              ,NVL(S.���N���i04,0) AS ���N���i04                        
              ,NVL(S.���N���i05,0) AS ���N���i05                        
              ,NVL(S.���N���i06,0) AS ���N���i06                        
              ,NVL(S.���N���i07,0) AS ���N���i07                        
              ,NVL(S.���N���i08,0) AS ���N���i08                        
              ,NVL(S.���N���i09,0) AS ���N���i09                        
              ,NVL(S.���N���i10,0) AS ���N���i10                        
              ,NVL(S.���N���i11,0) AS ���N���i11                        
              ,NVL(S.���N���i12,0) AS ���N���i12                        
              ,NVL(S.���N���i���,0) AS ���N���i���                    
              ,NVL(S.���N���i����,0) AS ���N���i����                    
              ,NVL(S.���N���i�݌v,0) AS ���N���i�݌v                    
              ,NVL(S.���N����01,0) AS ���N����01                        
              ,NVL(S.���N����02,0) AS ���N����02                        
              ,NVL(S.���N����03,0) AS ���N����03                        
              ,NVL(S.���N����04,0) AS ���N����04                        
              ,NVL(S.���N����05,0) AS ���N����05                        
              ,NVL(S.���N����06,0) AS ���N����06                        
              ,NVL(S.���N����07,0) AS ���N����07                        
              ,NVL(S.���N����08,0) AS ���N����08                        
              ,NVL(S.���N����09,0) AS ���N����09                        
              ,NVL(S.���N����10,0) AS ���N����10                        
              ,NVL(S.���N����11,0) AS ���N����11                        
              ,NVL(S.���N����12,0) AS ���N����12                        
              ,NVL(S.���N�������,0) AS ���N�������                    
              ,NVL(S.���N��������,0) AS ���N��������                    
              ,NVL(S.���N�����݌v,0) AS ���N�����݌v                    
              ,NVL(S.���N�`��01,0) AS ���N�`��01                        
              ,NVL(S.���N�`��02,0) AS ���N�`��02                        
              ,NVL(S.���N�`��03,0) AS ���N�`��03                        
              ,NVL(S.���N�`��04,0) AS ���N�`��04                        
              ,NVL(S.���N�`��05,0) AS ���N�`��05                        
              ,NVL(S.���N�`��06,0) AS ���N�`��06                        
              ,NVL(S.���N�`��07,0) AS ���N�`��07                        
              ,NVL(S.���N�`��08,0) AS ���N�`��08                        
              ,NVL(S.���N�`��09,0) AS ���N�`��09                        
              ,NVL(S.���N�`��10,0) AS ���N�`��10                        
              ,NVL(S.���N�`��11,0) AS ���N�`��11                        
              ,NVL(S.���N�`��12,0) AS ���N�`��12                        
              ,NVL(S.���N�`�ԏ��,0) AS ���N�`�ԏ��                    
              ,NVL(S.���N�`�ԉ���,0) AS ���N�`�ԉ���                    
              ,NVL(S.���N�`�ԗ݌v,0) AS ���N�`�ԗ݌v                    
              ,NVL(S.���N�a��01,0) AS ���N�a��01                        
              ,NVL(S.���N�a��02,0) AS ���N�a��02                        
              ,NVL(S.���N�a��03,0) AS ���N�a��03                        
              ,NVL(S.���N�a��04,0) AS ���N�a��04                        
              ,NVL(S.���N�a��05,0) AS ���N�a��05                        
              ,NVL(S.���N�a��06,0) AS ���N�a��06                        
              ,NVL(S.���N�a��07,0) AS ���N�a��07                        
              ,NVL(S.���N�a��08,0) AS ���N�a��08                        
              ,NVL(S.���N�a��09,0) AS ���N�a��09                        
              ,NVL(S.���N�a��10,0) AS ���N�a��10                        
              ,NVL(S.���N�a��11,0) AS ���N�a��11                        
              ,NVL(S.���N�a��12,0) AS ���N�a��12                        
              ,NVL(S.���N�a�ԏ��,0) AS ���N�a�ԏ��                    
              ,NVL(S.���N�a�ԉ���,0) AS ���N�a�ԉ���                    
              ,NVL(S.���N�a�ԗ݌v,0) AS ���N�a�ԗ݌v                    
              ,NVL(S.���N���01,0) AS ���N���01                        
              ,NVL(S.���N���02,0) AS ���N���02                        
              ,NVL(S.���N���03,0) AS ���N���03                        
              ,NVL(S.���N���04,0) AS ���N���04                        
              ,NVL(S.���N���05,0) AS ���N���05                        
              ,NVL(S.���N���06,0) AS ���N���06                        
              ,NVL(S.���N���07,0) AS ���N���07                        
              ,NVL(S.���N���08,0) AS ���N���08                        
              ,NVL(S.���N���09,0) AS ���N���09                        
              ,NVL(S.���N���10,0) AS ���N���10                        
              ,NVL(S.���N���11,0) AS ���N���11                        
              ,NVL(S.���N���12,0) AS ���N���12                        
              ,NVL(S.���N��ԏ��,0) AS ���N��ԏ��                    
              ,NVL(S.���N��ԉ���,0) AS ���N��ԉ���                    
              ,NVL(S.���N��ԗ݌v,0) AS ���N��ԗ݌v                    
              ,NVL(S.���N������01,0) AS ���N������01                    
              ,NVL(S.���N������02,0) AS ���N������02                    
              ,NVL(S.���N������03,0) AS ���N������03                    
              ,NVL(S.���N������04,0) AS ���N������04                    
              ,NVL(S.���N������05,0) AS ���N������05                    
              ,NVL(S.���N������06,0) AS ���N������06                    
              ,NVL(S.���N������07,0) AS ���N������07                    
              ,NVL(S.���N������08,0) AS ���N������08                    
              ,NVL(S.���N������09,0) AS ���N������09                    
              ,NVL(S.���N������10,0) AS ���N������10                    
              ,NVL(S.���N������11,0) AS ���N������11                    
              ,NVL(S.���N������12,0) AS ���N������12                    
              ,NVL(S.���N���������,0) AS ���N���������                
              ,NVL(S.���N����������,0) AS ���N����������                
              ,NVL(S.���N�������݌v,0) AS ���N�������݌v                
              ,NVL(S.���N�ӔC�z01,0) AS ���N�ӔC�z01                    
              ,NVL(S.���N�ӔC�z02,0) AS ���N�ӔC�z02                    
              ,NVL(S.���N�ӔC�z03,0) AS ���N�ӔC�z03                    
              ,NVL(S.���N�ӔC�z04,0) AS ���N�ӔC�z04                    
              ,NVL(S.���N�ӔC�z05,0) AS ���N�ӔC�z05                    
              ,NVL(S.���N�ӔC�z06,0) AS ���N�ӔC�z06                    
              ,NVL(S.���N�ӔC�z07,0) AS ���N�ӔC�z07                    
              ,NVL(S.���N�ӔC�z08,0) AS ���N�ӔC�z08                    
              ,NVL(S.���N�ӔC�z09,0) AS ���N�ӔC�z09                    
              ,NVL(S.���N�ӔC�z10,0) AS ���N�ӔC�z10                    
              ,NVL(S.���N�ӔC�z11,0) AS ���N�ӔC�z11                    
              ,NVL(S.���N�ӔC�z12,0) AS ���N�ӔC�z12                    
              ,NVL(S.���N�ӔC�z���,0) AS ���N�ӔC�z���                
              ,NVL(S.���N�ӔC�z����,0) AS ���N�ӔC�z����                
              ,NVL(S.���N�ӔC�z�݌v,0) AS ���N�ӔC�z�݌v                
             FROM                                                       
              (                                                         
              SELECT                                                    
                 T1.���Ӑ�R�[�h�}�e                                    
                ,T1.�����a�j||'0' AS �����a�j0                          
                ,T2.�u�����h�R�[�h                                      
                ,T2.������                                            
                ,T2.����N����                                          
                ,T2.���N����                                          
                ,T1.�����ԑ���                                        
                ,T1.����N��������                                      
                ,T1.���N��������                                      
                ,T1.�`���l���敪                                        
                ,T1.�V�`���l���敪                                      
                ,T1.�r�q�敪                                            
                ,T1.�N���X�R�[�h                                        
                ,T1.�ƑԂQ�敪                                          
                ,T1.�̎����敪                                          
                ,T1.���ߑ��E                                            
                ,T3.�������@                                            
                ,T3.�x��������`�L�[                                    
                ,T3.���������Ώ�                                        
                ,T3.�������t�H�[�}�b�g                                  
                                                                        
                ,CASE WHEN SUBSTR(T2.����N����,1,6) =                  
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x�V�K�敪         
                ,CASE WHEN SUBSTR(T2.����N����,1,6) BETWEEN            
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v�V�K�敪         
                ,CASE WHEN SUBSTR(T2.����N����,1,6) =                  
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x�V�K�敪         
                ,CASE WHEN SUBSTR(T2.����N����,1,6) BETWEEN            
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v�V�K�敪         
                                                                        
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) =              
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x�V�K�敪����     
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) BETWEEN        
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v�V�K�敪����     
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) =              
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x�V�K�敪����     
                ,CASE WHEN SUBSTR(T1.����N��������,1,6) BETWEEN        
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v�V�K�敪����     
                                                                        
                ,CASE WHEN SUBSTR(T2.���N����,1,6) =                  
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x���敪         
                ,CASE WHEN SUBSTR(T2.���N����,1,6) BETWEEN            
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v���敪         
                ,CASE WHEN SUBSTR(T2.���N����,1,6) =                  
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x���敪         
                ,CASE WHEN SUBSTR(T2.���N����,1,6) BETWEEN            
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v���敪         
                                                                        
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) =              
                      :WK�O�N���o�I���N��                               
                      THEN '1' ELSE '0' END AS �O�N���x���敪����     
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) BETWEEN        
                      :WK�O�N���o�J�n�N�� AND :WK�O�N���o�I���N��       
                      THEN '1' ELSE '0' END AS �O�N�݌v���敪����     
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) =              
                      :WK���N���o�I���N��                               
                      THEN '1' ELSE '0' END AS ���N���x���敪����     
                ,CASE WHEN SUBSTR(T1.���N��������,1,6) BETWEEN        
                      :WK���N���o�J�n�N�� AND :WK���N���o�I���N��       
                      THEN '1' ELSE '0' END AS ���N�݌v���敪����     
               FROM                                                     
                  HAT_TOKUI_KIHON  T1                                   
                , HCV_TOKUI_BRAND_CVS  T2                               
                , HAT_TOKUI_SYOSAI T3                                   
               WHERE                                                    
                     T1.�Ώ۔N�� = :WK�Ώ۔N���x                        
                 AND T1.�Ώۂ`�a�敪 = :WK�Ώۂ`�a�敪                  
                 AND T1.���Ӑ�R�[�h = T1.���Ӑ�R�[�h�}�e              
                 AND T2.���Ӑ�R�[�h = T2.���Ӑ�R�[�h�}�e              
                 AND T3.���Ӑ�R�[�h = T3.���Ӑ�R�[�h�}�e           
                 AND T2.�u�����h�R�[�h NOT IN ('02', '20', '21', '23',
                                               '13', '66','30',
                                               '80', '81', '82')        
      *           AND T2.������ IN ('0', '1', '2', '3')                 
                 AND T1.���Ӑ�R�[�h = T2.���Ӑ�R�[�h                  
                 AND T1.�Ώ۔N�� = T2.�Ώ۔N��                          
                 AND T1.�Ώۂ`�a�敪 = T2.�Ώۂ`�a�敪                  
                 AND T1.���Ӑ�R�[�h = T3.���Ӑ�R�[�h                  
                 AND T1.�Ώ۔N�� = T3.�Ώ۔N��                          
                 AND T1.�Ώۂ`�a�敪 = T3.�Ώۂ`�a�敪                  
              ) T                                                       
             ,(                                                         
              SELECT                                                    
                 S1.���Ӑ�R�[�h�}�e                                    
                ,NVL(D1.�u�����h�ϊ��R�[�h,S1.�u�����h�R�[�h)           
                  AS �u�����h�R�[�h                                     
       /* �O�N���i */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i01                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i02                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i03                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i04                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i05                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i06                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i07                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i08                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i09                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                   THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END) 
                  AS �O�N���i10                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i11                                         
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i12                                         
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i���                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i����                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS �O�N���i�݌v                                       
                                                                        
       /* �O�N���� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����01          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����02          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����03          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����04          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����05          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����06          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����07          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����08          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����09          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����10          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����11          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�����i���z ELSE 0 END) AS �O�N����12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�����i���z ELSE 0 END) AS �O�N�������        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS �O�N��������        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS �O�N�����݌v        
                                                                        
       /* �O�N�`�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��01          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��02          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��03          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��04          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��05          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��06          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��07          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��08          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��09          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��10          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��11          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS �O�N�`�ԗ݌v        
                                                                        
       /* �O�N�a�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��01          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��02          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��03          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��04          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��05          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��06          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��07          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��08          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��09          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��10          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��11          
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS �O�N�a�ԗ݌v        
                                                                        
       /* �O�N��� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���01        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���02        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���03        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���04        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���05        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���06        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���07        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���08        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���09        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���10        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���11        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N���12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N��ԏ��      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N��ԉ���      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS �O�N��ԗ݌v      
                                                                        
       /* �O�N������ */                                                 
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'01'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������01        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'02'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������02        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'03'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������03        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'04'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������04        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'05'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������05        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'06'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������06        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'07'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������07        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'08'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������08        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x||'09'        
                  THEN S1.���������z ELSE 0 END) AS �O�N������09        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'10'     
                  THEN S1.���������z ELSE 0 END) AS �O�N������10        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'11'     
                  THEN S1.���������z ELSE 0 END) AS �O�N������11        
                ,SUM(CASE WHEN S1.������N�� = :WK�O�N�x ||'12'     
                  THEN S1.���������z ELSE 0 END) AS �O�N������12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x||'06'                
                  THEN S1.���������z ELSE 0 END) AS �O�N���������      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'07'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS �O�N����������      
                ,SUM(CASE WHEN S1.������N�� >= :WK�O�N�x||'01'       
                  AND S1.������N�� <= :WK�O�N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS �O�N�������݌v      
                                                                        
       /* �O�N�ӔC�z */                                                 
                ,'0' AS �O�N�ӔC�z01                                    
                ,'0' AS �O�N�ӔC�z02                                    
                ,'0' AS �O�N�ӔC�z03                                    
                ,'0' AS �O�N�ӔC�z04                                    
                ,'0' AS �O�N�ӔC�z05                                    
                ,'0' AS �O�N�ӔC�z06                                    
                ,'0' AS �O�N�ӔC�z07                                    
                ,'0' AS �O�N�ӔC�z08                                    
                ,'0' AS �O�N�ӔC�z09                                    
                ,'0' AS �O�N�ӔC�z10                                    
                ,'0' AS �O�N�ӔC�z11                                    
                ,'0' AS �O�N�ӔC�z12                                    
                ,'0' AS �O�N�ӔC�z���                                  
                ,'0' AS �O�N�ӔC�z����                                  
                ,'0' AS �O�N�ӔC�z�݌v                                  
                                                                        
       /* ���N���i */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i01                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i02                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i03                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i04                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i05                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i06                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i07                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i08                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i09                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i10                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i11                                         
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i12                                         
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i���                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i����                                       
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z+�`�ԕi���z+�a�ԕi���z ELSE 0 END)  
                  AS ���N���i�݌v                                       
                                                                        
       /* ���N���� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����01          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����02          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����03          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����04          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����05          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����06          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����07          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����08          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�����i���z ELSE 0 END) AS ���N����09          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�����i���z ELSE 0 END) AS ���N����10          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�����i���z ELSE 0 END) AS ���N����11          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�����i���z ELSE 0 END) AS ���N����12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�����i���z ELSE 0 END) AS ���N�������        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS ���N��������        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�����i���z ELSE 0 END) AS ���N�����݌v        
                                                                        
       /* ���N�`�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��01          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��02          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��03          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��04          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��05          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��06          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��07          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��08          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��09          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��10          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��11          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�`�ԕi���z ELSE 0 END) AS ���N�`�ԗ݌v        
                                                                        
       /* ���N�a�� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��01          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��02          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��03          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��04          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��05          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��06          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��07          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��08          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��09          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��10          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��11          
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a��12          
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a�ԏ��        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a�ԉ���        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.�a�ԕi���z ELSE 0 END) AS ���N�a�ԗ݌v        
                                                                        
       /* ���N��� */                                                   
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���01        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���02        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���03        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���04        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���05        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���06        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���07        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���08        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���09        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���10        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���11        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N���12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N��ԏ��      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N��ԉ���      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���ԕi���z ELSE 0 END) AS ���N��ԗ݌v      
                                                                        
       /* ���N������ */                                                 
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'01'        
                  THEN S1.���������z ELSE 0 END) AS ���N������01        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'02'        
                  THEN S1.���������z ELSE 0 END) AS ���N������02        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'03'        
                  THEN S1.���������z ELSE 0 END) AS ���N������03        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'04'        
                  THEN S1.���������z ELSE 0 END) AS ���N������04        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'05'        
                  THEN S1.���������z ELSE 0 END) AS ���N������05        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'06'        
                  THEN S1.���������z ELSE 0 END) AS ���N������06        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'07'        
                  THEN S1.���������z ELSE 0 END) AS ���N������07        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'08'        
                  THEN S1.���������z ELSE 0 END) AS ���N������08        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x||'09'        
                  THEN S1.���������z ELSE 0 END) AS ���N������09        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'10'     
                  THEN S1.���������z ELSE 0 END) AS ���N������10        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'11'     
                  THEN S1.���������z ELSE 0 END) AS ���N������11        
                ,SUM(CASE WHEN S1.������N�� = :WK���N�x ||'12'     
                  THEN S1.���������z ELSE 0 END) AS ���N������12        
                                                                        
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x||'06'                
                  THEN S1.���������z ELSE 0 END) AS ���N���������      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'07'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS ���N����������      
                ,SUM(CASE WHEN S1.������N�� >= :WK���N�x||'01'       
                  AND S1.������N�� <= :WK���N�x ||'12'             
                  THEN S1.���������z ELSE 0 END) AS ���N�������݌v      
                                                                        
       /* ���N�ӔC�z */                                                 
                ,'0' AS ���N�ӔC�z01                                    
                ,'0' AS ���N�ӔC�z02                                    
                ,'0' AS ���N�ӔC�z03                                    
                ,'0' AS ���N�ӔC�z04                                    
                ,'0' AS ���N�ӔC�z05                                    
                ,'0' AS ���N�ӔC�z06                                    
                ,'0' AS ���N�ӔC�z07                                    
                ,'0' AS ���N�ӔC�z08                                    
                ,'0' AS ���N�ӔC�z09                                    
                ,'0' AS ���N�ӔC�z10                                    
                ,'0' AS ���N�ӔC�z11                                    
                ,'0' AS ���N�ӔC�z12                                    
                ,'0' AS ���N�ӔC�z���                                  
                ,'0' AS ���N�ӔC�z����                                  
                ,'0' AS ���N�ӔC�z�݌v                                  
                                                                        
               FROM                                                     
                  HCT_SALES_PERFORMANCE  S1                             
                ,(SELECT '02' AS �u�����h�R�[�h                         
                        ,'01' AS �u�����h�ϊ��R�[�h FROM dual           
                  UNION ALL                                             
                  SELECT '20','19' FROM dual                            
                  UNION ALL                                             
                  SELECT '21','10' FROM dual                            
       /* 2011.10.20 ADD �ْ߰ǉ� */   
                  UNION ALL                                             
                  SELECT '66','65' FROM dual                            
       /* 2011.11.17 ADD CVS50�ǉ� */   
                  UNION ALL                                             
                  SELECT '30','12' FROM dual                            
                  UNION ALL                                             
                  SELECT '23','22' FROM dual                            
                 ) D1                                                   
               WHERE                                                    
                     S1.�u�����h�R�[�h = D1.�u�����h�R�[�h (+)          
                 AND S1.�u�����h�R�[�h NOT IN ('90', '91', '92')        
                 AND (  (   S1.������N�� >= :WK�O�N���o�J�n�N��      
                        AND S1.������N�� <= :WK�O�N���o�I���N��)     
                     OR (   S1.������N�� >= :WK���N���o�J�n�N��      
                        AND S1.������N�� <= :WK���N���o�I���N��) )   
               GROUP BY                                                 
                     S1.���Ӑ�R�[�h�}�e                                
                    ,NVL(D1.�u�����h�ϊ��R�[�h,S1.�u�����h�R�[�h)       
              ) S                                                       
                                                                        
             WHERE                                                      
                   T.���Ӑ�R�[�h�}�e = S.���Ӑ�R�[�h�}�e (+)          
               AND T.�u�����h�R�[�h = S.�u�����h�R�[�h (+)              
                                                                        
           END-EXEC.                                                    
           MOVE  SQLERRD(3)  TO  INSERT-CNT.                            
                                                                        
           MOVE '�䒠�ǉ������Q����' TO D-���b�Z�[�W���.               
           CALL KXU002SC   USING  D-���O���.                           
                                                                        
      *-----------------------------------------------------------------
       SQL-ERRORI.                                                      
      *-----------------------------------------------------------------
           MOVE  '�r�p�k�G���[ �䒠�ǉ�����' TO D-���b�Z�[�W���.       
           CALL  KXU002SC  USING  D-���O���.                           
           EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.                
           MOVE 'ORACLE ERROR DETECTED:' TO D-���b�Z�[�W���.           
           MOVE SQLERRMC TO D-���b�Z�[�W���(25:100).                   
           CALL KXU002SC USING D-���O���.                              
                                                                        
           EXEC SQL AT :DB-NAME ROLLBACK WORK RELEASE END-EXEC.         
                                                                        
           MOVE ABND-CODE TO RETURN-CODE                                
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
       �I������.                                                        
      *-----------------------------------------------------------------
           EXEC SQL AT :DB-NAME COMMIT WORK RELEASE END-EXEC.           
           MOVE 'DELETE   ='   TO D-�J�E���g���o��                      
           MOVE DELETE-CNT     TO D-�J�E���g                            
           CALL KXU002SC    USING D-���O���                            
           MOVE 'INSERT   ='   TO D-�J�E���g���o��                      
           MOVE INSERT-CNT     TO D-�J�E���g                            
           CALL KXU002SC    USING D-���O���                            
           MOVE 'END       '   TO D-�J�E���g���o��.                     
           CALL KXU002SC   USING  D-���O���.                           
           STOP RUN.                                                    
                                                                        
      *-----------------------------------------------------------------
