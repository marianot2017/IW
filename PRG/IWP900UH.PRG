*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : Hacer copia de seguridad.
*-H 
*-H	Release			: 01
*-H Fecha de inicio : 
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref
	
PRIVATE ALL LIKE w_*

if parameters()=0
	w_pref=''
endif

w_alta=.f.

** Salvado de estados anteriores 

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

** Variables internas
w_opcion='Confirma'
w_prog=program()
w_pfile='900uh'

** Datos del archivo a mantener
w_title='COPIA DE SEGURIDAD (RESGUARDO DE '

if w_pref='C'

	set escape off
	
	wait wind 'El resguardo de sistema completo debera hacerlo desde el util'
	
	read when .f.
	
else	

	do case
		case w_pref='E'
			w_title=w_title+'EMPRESA)'
		case w_pref='S'
			w_title=w_title+'ARCH.SISTEMA)'
	endcase

	set skip of menu _msysmenu .t.

	w_unid=asc(left(sys(5),1))-64
	w_letr=sys(5)
	w_path=''

	=closall()

	do iwm900uf.mpr

	do iws900uh.spr
	
endif

pop key  
do ssm995.mpr	&& R.01 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu

=closall()

return


*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    Procedure Conf900uh
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w0mop$'D'
	=advsuave('Versi๓n Demostraci๓n. Inhibido el acceso.')
	read when .f.
	return
endif

if .not. file('\IW\azip32.dll')
   wait ' No se encuentra el AZIP32.DLL en el directorio actual ' wind
   return
endif   

* Compacta 
*          ARJ A : agrega archivos
*          -VA   : en volumenes segun el espacio detecte en la diketera
*          -R    : compacta tambien los subdirectorios
*          -Y    : no pregunta si crea un nuevo directorio al compactar
*		   -HF 	 : no graba con nombres largos propio de windows 95
*          -XARCHIVO : Excluye de la compactacion los archivos indicados

w_para='\IW\ARJ A -HF -Y -VA '		&& uso el arj.exe del raiz iw que esta actualizado

w_letr=chr(64+w_unid)+':'	&& ej= 64+1=65 -> chr(65)=A 
if !dispOk(w_letr)
	=advsuave('Dispositivo Destino No Vแlido ')
 	return
endif

if sino('Confirma generacion de copia de seguridad ?','No',iif(w_pref='E','Resguardo de Empresa','Resguardo de Archivos de Sistema'))='S'
    ** Verifico espacio en disco **

    ***vfp***
	w_defa=sys(5)+sys(2003)
	***vfp***
    
    w_letr=chr(64+w_unid)+':'	&& ej= 64+1=65 -> chr(65)=A 
 
    set defa to &w_letr
    
    if diskspace()<12800
        wait 'No hay espacio suficiente en el dispositivo' wind nowait	
        set defa to &w_defa
        return
    endif

    set defa to &w_defa


    * crea segun la disketera un archivo IW.arj, IW.A01 etc. de todos 
    * archivos con extencion .dbf y .fpt

	do case
	case w_pref='E'	&& empresa
	    w_para=w_para+w_letr+'\'+d0pref+d0coem+' '+d0coem+'\ *.DBF *.FPT'+' \IW\VER*.*'
    
    	w_wind=wontop()
	    save wind &w_wind to &d0f99
    
    	deacti wind &w_wind
    
	    set resource off

		if file('iw.mem')
		   rest from iw.mem additive
		else
		   s_text='Instalacion Incompleta, falta archivo de claves'
		   do iws900am.spr
		   return .f.
		endif

*		x_d0inst=d0inst
		x_d0opci=d0opci
		x_w0mop=w0mop
*		x_d0desh=d0desh
		x_d0emp=d0emp
*		x_d0vers=d0vers

		if type('Z_NC')=='C'
			clav2 = decript(z_nc,3)
			x_wacla = subst(clav2,18+asc(subst(clav2,17,1))+1,5)
		else
			x_wacla = space(5)
		endif

		if type('Z_DA')=='C'
			x_date = decript(z_da,3)
			x_date = subst(x_date,18+asc(subst(x_date,17,1))+1,8)
			x_date = ctod(right(x_date,2)+'/'+substr(x_date,5,2)+'/'+left(x_date,4))
		else
			x_date = {}
		endif
		
		w_empr='   '
		w_form='99'
		w_driv=w_letr
		w_conf=1
		w_path=''
		w_cd=sys(5)+curdir()
		w_sys=d0pref

		save to &w_cd.backup.mem all like x_*

		priv w_title
		w_title='Resguardo de Empresa'

		sele 0
		use ivf010 alias 'Fempr' order p0coem shared 
		if neterr()
			wait wind 'No se puede realizar el Resguardo' nowait
		else
			w_part=0
			
			if !utildrem()	
				w_conf=0
			endif
			
			w_empr=allt(d0coem)
			
			if w_conf=1 and lastkey()<>p_esc
				if right(w_path,1)!='\'
					w_path=w_path+'\'
				endif
				=seek(w_empr,'Fempr')
				if sino(' Confirma Resguardo de Empresa '+fempr.p0coem+'-'+allt(fempr.p0empr)+' ? ','N','De '+w_cd+w_empr+' hacia  '+w_path)='S'
				
					if seek(w_empr,'Fempr')
						Empr_Ver=w_cd+'Empr_Ver.dbf'
						sele 0
						***vfp***
						if version()='Visual FoxPro'
							Empr_Verx=w_cd+'Empr_Verx.dbf'
							create dbf &Empr_Verx. (p0coem c(len(w_form)), p0veda c(3), p0memo l(1))
							copy to &Empr_Ver. type fox2x
							use &Empr_Ver.
							erase &Empr_Verx..*
						else
						***vfp***
							create dbf &Empr_Ver. (p0coem c(len(w_form)), p0veda c(3), p0memo l(1))
						endif
						insert into &Empr_Ver. (p0coem, p0veda, p0memo) values (w_empr, Fempr.p0veda, iif(mfile('\aw\aif000.dbf'),.t.,.f.))
						use in Empr_Ver
					else
						Empr_Ver=''
						loop						
					endif

					erase &w_path&w_sys&w_empr.VFP.001
	
					if used('f77')
						sele f77
						use
					endif

					**no ejecutaba en Windows 2000 porque el limite del parametro es 127 caracteres
					**no ejecutaba en XP, aparentemente es menor de 127 caracteres, por eso lo particione.
					**primero guardo lo que entra en un disquet y luego agrego lo demas que puede ocupar mas de un disquet.
					

					set defa to &w_path
					if val(sys(2020))<1500000	&& Disketera
						w_part=0
					endif
					set defa to &w_cd

					wait wind chr(13)+chr(13)+'Resguardando en : '+w_path+w_sys+w_empr+'VFP.001'+chr(13)+chr(13) nowait
					
					=delefile(w_path,w_sys+w_empr+'VFP.*')
					
					wVerCant = ADIR(VerFiles,'\' + d0pref + '\V???????.*')
					private i, xfiles
					for i=1 to wvercant
					  	xfiles=right(verfiles(i,1),3)
					  	if !(isdigit(substr(xfiles,1,1)) and isdigit(substr(xfiles,2,1)) and isdigit(substr(xfiles,3,1)))
							=adel(verfiles,i)
							wvercant=wvercant - 1
							i=i-1
						endif
						if wvercant>0
							dimension verfiles(wvercant,5)
						endif
						if i = wvercant
							exit
						endif
				  	endfor

					private w_date,w_time,w_verif
					w_date=date()
					w_time=time()
					dimension w_verif(1,5)
					w_verif=''

					private w_fzip,w_finc,w_fexc,w_vfox,w_inst001,w_inst002

					w_fzip=w_path+w_sys+w_empr+'VFP.001'
					w_finc='*.dbf|'+'*.fpt|'+'*.dbt|'+'Empr_Ver|'+'backup.mem|'+verfiles(1,1)
					w_fexc=''

					erase &w_path._bkfin.dbf	
		 			w_inst001='wait wind "" time 1'
					w_inst002='=pkzip("'+w_fzip+'","'+w_finc+'","'+w_fexc+'","'+'0'+'","'+'1'+'","'+'","'+'2'+'","'+str(w_part,1)+'","'+w_path+'_bkfin.dbf'+'")'
					private d0f9fe
					d0f9fe=getenv('temp')+'\'+sys(3)
					w_vfox=d0f9fe+'.mem'
					save to &w_vfox. all like w_inst0*
					release all like w_inst0*
					=visuafox('ejeproc(.f.)',w_vfox,'Resguardo de Empresa',1)

					i=0	
					do while .t.	
						wait wind chr(13)+chr(13)+'Resguardando en : '+w_fzip+chr(13)+replicate('',i)+chr(13) time 1
						if len('Resguardando en : '+w_fzip)<=i
							i=1
						else
							i=i+1
						endif
						if file(w_path+'_bkfin.dbf') and file(w_path+w_sys+w_empr+'VFP.001')
							=adir(w_verif,w_path+w_sys+w_empr+'VFP.001')
							if w_verif(1,1)=w_sys+w_empr+'VFP.001'
								if dtos(w_verif(1,3))+strtran(w_verif(1,4),' ','0')>=dtos(w_date)+w_time and w_verif(1,2)>100
									exit
								endif
							endif
						endif
						if lastkey()=p_esc
							if sino('El resguardo en '+w_fzip+' esta llevando tiempo. La demora depende del volumen de informaci๓n a compactar. Sigue aguardando?','Si','Resguardo de Empresa')!='S'
								=advsuave('Va a liberar el proceso, la compactaci๓n sigue corriendo en otra ventana. No cierre esa ventana, vamos a darle mas tiempo para terminar el resguardo.','Resguardo de Empresa')
								exit
							else
								keyboard '{enter}'
								=inkey()
							endif
						endif
					enddo
					
					if lastkey()!=p_esc
						erase &w_path._bkfin.dbf
						w_date=date()
						w_time=time()
						dimension w_verif(1,5)
						w_verif=''
					
						w_fzip=w_path+w_sys+w_empr+'VFP.001'
						w_finc=w_empr+'\*.dbf|'+w_empr+'\*.fpt|'+w_empr+'\*.bmp|'+w_empr+'\*.jpg|'
						w_fexc=''

		 				w_inst001='wait wind "" time 1'
						w_inst002='=pkzip("'+w_fzip+'","'+w_finc+'","'+w_fexc+'","'+'0'+'","'+'1'+'","'+'","'+'2'+'","'+str(w_part,1)+'","'+w_path+'_bkfin.dbf'+'")'
						private d0f9fe
						d0f9fe=getenv('temp')+'\'+sys(3)
						w_vfox=d0f9fe+'.mem'
						save to &w_vfox. all like w_inst0*
						release all like w_inst0*
						=visuafox('ejeproc(.f.)',w_vfox,'Resguardo de Empresa',1)

						i=0	
						do while .t.
							wait wind chr(13)+chr(13)+'Resguardando en : '+w_fzip+chr(13)+replicate('',i)+chr(13) time 1
							if len('Resguardando en : '+w_fzip)<=i
								i=1
							else
								i=i+1
							endif
							if file(w_path+'_bkfin.dbf') and file(w_path+w_sys+w_empr+'VFP.001')
								=adir(w_verif,w_path+w_sys+w_empr+'VFP.001')
								if w_verif(1,1)=w_sys+w_empr+'VFP.001'
									if dtos(w_verif(1,3))+strtran(w_verif(1,4),' ','0')>=dtos(w_date)+w_time and w_verif(1,2)>1000
										exit
									endif
								endif
							endif
							if lastkey()=p_esc
								if sino('El resguardo en '+w_fzip+' esta llevando tiempo. La demora depende del volumen de informaci๓n a compactar. Sigue aguardando?','Si','Resguardo de Empresa')!='S'
									=advsuave('Va a liberar el proceso, la compactaci๓n sigue corriendo en otra ventana. No cierre esa ventana, vamos a darle mas tiempo para terminar el resguardo.','Resguardo de Empresa')
									exit
								else
									keyboard '{enter}'
									=inkey()
								endif
							endif
						enddo
					
					endif
			
					if file(w_vfox) and file(w_path+'_bkfin.dbf')  and file(w_path+w_sys+w_empr+'VFP.001')
						restore from &w_vfox. additive
						erase &w_vfox.
						if type('p_resu')='C' and len(allt(p_resu))>0
							wait wind p_resu nowait
						else
							wait wind chr(13)+chr(13)+'Termino Resguardo de Empresa Exitosamente'+chr(13)+chr(13) time 2
						endif
					else
						wait wind chr(13)+chr(13)+'No Termino Resguardo de Empresa'+chr(13)+chr(13) time 2
					endif
					erase &v_fox
					rele d0f9fe
					erase &w_path._bkfin.dbf

					=delefile(w_cd,'Empr_Ver.dbf')
					=delefile(w_cd,'backup.mem')
					
					=login('S')
					=login('E')
				else
					w_conf=0
				endif
			endif

			use in Fempr
		endif
	    
	    set resource on

		w_back=adir(w_file,w_path+d0pref+d0coem+'VFP.*')
		
	case w_pref='S'	&& archivos sistema
	
	    w_para=w_para+w_letr+'\'+d0pref+'SISVFP'+' *.DBF *.FPT'+' VER*.*'
    
    	w_wind=wontop()
	    save wind &w_wind to &d0f99
    
    	deacti wind &w_wind
    
	    set resource off

		if file('iw.mem')
		   rest from iw.mem additive
		else
		   s_text='Instalacion Incompleta, falta archivo de claves'
		   do iws900am.spr
		   return .f.
		endif

*		x_d0inst=d0inst
		x_d0opci=d0opci
		x_w0mop=w0mop
*		x_d0desh=d0desh
		x_d0emp=d0emp
*		x_d0vers=d0vers

		if type('Z_DA')=='C'
			clav2=decript(Z_NC,3)
			x_wacla=subst(clav2,18+asc(subst(clav2,17,1))+1,4)
		else
			x_wacla=''
		endif
		if type('Z_NC')=='C'
			clav2 = decript(z_nc,3)
			x_wacla = subst(clav2,18+asc(subst(clav2,17,1))+1,5)
		else
			x_wacla = space(5)
		endif

		if type('Z_DA')=='C'
			x_date = decript(z_da,3)
			x_date = subst(x_date,18+asc(subst(x_date,17,1))+1,8)
			x_date = ctod(right(x_date,2)+'/'+substr(x_date,5,2)+'/'+left(x_date,4))
		else
			x_date = {}
		endif
		
		w_conf=1
		
		w_empr='SIS'
		w_form='99'
		w_driv=w_letr
		w_conf=1
		w_path=''
		w_cd=sys(5)+curdir()
		w_sys=d0pref

		save to &w_cd.backup.mem all like x_*

		priv w_title
		w_title='Resguardo de Archivos de Sistema'

		sele 0
		use ivf010 alias 'Fempr' order p0coem shared 
		if neterr()
			wait wind 'No se puede realizar el Resguardo' nowait
		else
			w_part=0
			
			if !utildrem()
				w_conf=0
			endif
			
			if w_conf=1 and lastkey()<>p_esc
				if right(w_path,1)!='\'
					w_path=w_path+'\'
				endif
				if sino(' Confirma Resguardo de Archivos de Sistema? ','N','De '+w_cd+' hacia  '+w_path)='S'
				
					erase &w_path&w_sys&w_empr.VFP.001
	
					if used('f77')
						sele f77
						use
					endif

					**no ejecutaba en Windows 2000 porque el limite del parametro es 127 caracteres
					**no ejecutaba en XP, aparentemente es menor de 127 caracteres, por eso lo particione.
					**primero guardo lo que entra en un disquet y luego agrego lo demas que puede ocupar mas de un disquet.
					
					set defa to &w_path
					if val(sys(2020))<1500000	&& Disketera
						w_part=0
					endif
					set defa to &w_cd

					wait wind chr(13)+chr(13)+'Resguardando en : '+w_path+w_sys+w_empr+'VFP.001'+chr(13)+chr(13) nowait
					
					=delefile(w_path,w_sys+w_empr+'VFP.*')
					
					wVerCant = ADIR(VerFiles,'\' + d0pref + '\V???????.*')
					private i, xfiles
					for i=1 to wvercant
					  	xfiles=right(verfiles(i,1),3)
					  	if !(isdigit(substr(xfiles,1,1)) and isdigit(substr(xfiles,2,1)) and isdigit(substr(xfiles,3,1)))
							=adel(verfiles,i)
							wvercant=wvercant - 1
							i=i-1
						endif
						if wvercant>0
							dimension verfiles(wvercant,5)
						endif
						if i = wvercant
							exit
						endif
				  	endfor

					private w_date,w_time,w_verif
					w_date=date()
					w_time=time()
					dimension w_verif(1,5)
					w_verif=''

					private w_fzip,w_finc,w_fexc,w_vfox,w_inst001,w_inst002

					w_fzip=w_path+w_sys+w_empr+'VFP.001'
					w_finc='*.dbf|'+'*.fpt|'+'*.dbt|'+'backup.mem|'+verfiles(1,1)
					w_fexc=''

		 			w_inst001='wait wind "" time 1'
					w_inst002='=pkzip("'+w_fzip+'","'+w_finc+'","'+w_fexc+'","'+'0'+'","'+'1'+'","'+'","'+'2'+'","'+str(w_part,1)+'","'+w_path+'_bkfin.dbf'+'")'
					private d0f9fe
					d0f9fe=getenv('temp')+'\'+sys(3)
					w_vfox=d0f9fe+'.mem'
					save to &w_vfox. all like w_inst0*
					release all like w_inst0*
					=visuafox('ejeproc(.f.)',w_vfox,'Resguardo de Archivos de Sistema',1)

					i=0	
					do while .t.	
						wait wind chr(13)+chr(13)+'Resguardando en : '+w_fzip+chr(13)+replicate('',i)+chr(13) time 1
						if len('Resguardando en : '+w_fzip)<=i
							i=1
						else
							i=i+1
						endif
						if file(w_path+'_bkfin.dbf') and file(w_path+w_sys+w_empr+'VFP.001')
							=adir(w_verif,w_path+w_sys+w_empr+'VFP.001')
							if w_verif(1,1)=w_sys+w_empr+'VFP.001'
								if dtos(w_verif(1,3))+strtran(w_verif(1,4),' ','0')>=dtos(w_date)+w_time and w_verif(1,2)>1000
									exit
								endif
							endif
						endif
						if lastkey()=p_esc
							if sino('El resguardo en '+w_fzip+' esta llevando tiempo. La demora depende del volumen de informaci๓n a compactar. Sigue aguardando?','Si','Resguardo de Archivos de Sistema')!='S'
								=advsuave('Va a liberar el proceso, la compactaci๓n sigue corriendo en otra ventana. No cierre esa ventana, vamos a darle mas tiempo para terminar el resguardo.','Resguardo de Archivos de Sistema')
								exit
							else
								keyboard '{enter}'
								=inkey()
							endif
						endif
					enddo
			
					if file(w_vfox) and file(w_path+'_bkfin.dbf') and file(w_path+w_sys+w_empr+'VFP.001')
						restore from &w_vfox. additive
						erase &w_vfox.
						if type('p_resu')='C' and len(allt(p_resu))>0
							wait wind p_resu nowait
						else
							wait wind chr(13)+chr(13)+'Termino Resguardo de Archivos de Sistema Exitosamente'+chr(13)+chr(13) time 2
						endif
					else
						wait wind chr(13)+chr(13)+'No Termino Resguardo de Archivos de Sistema'+chr(13)+chr(13) time 2
					endif
					erase &v_fox
					rele d0f9fe
					erase &w_path._bkfin.dbf

					=delefile(w_cd,'backup.mem')

					=login('S')
					=login('E')
				else
					w_conf=0
				endif
			endif

			use in Fempr
		endif
	    
	    set resource on

		w_back=adir(w_file,w_path+d0pref+'SISVFP.*')
	endcase

	if w_conf=1	
		if empty(w_back) 
		 	wait 'Resguardo de datos defectuoso.!!!'+chr(13)+;
	 			 'Debera volver a ejecutar la opcion' wind time 5
		else
			if w_file[1,2]=0 or w_file[1,3]<>date() or strtran(w_file[1,4],' ','0')>time()
			 	wait 'Resguardo de datos defectuoso.!!!'+chr(13)+;
		 			 'Debera volver a ejecutar la opcion' wind time 5
			else
			 	wait 'Resguardo de datos exitoso!!!' wind time 2
			endif
		endif
	endif	
    
    clear

    activ wind &w_wind
    restore wind &w_wind from &d0f99
    
    dele file &d0f99..win
    
    show gets disable
    
endif

return

*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNCTION utildrem
*-F ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_letr=chr(64+w_unid)+':'
W_PATH=w_letr+'\_bkmemo\'
a=dow(date())-1
do case
	case a=1
		w_path=w_path+'1_lun'
	case a=2
		w_path=w_path+'2_mar'
	case a=3
		w_path=w_path+'3_mie'
	case a=4
		w_path=w_path+'4_jue'
	case a=5
		w_path=w_path+'5_vie'
	case a=6
		w_path=w_path+'6_sab'
	case a=0
		w_path=w_path+'7_dom'
endcase

do case
	case !dispok(left(w_path,2))
		=advsuave('No se tiene permiso Read/Write a este directorio')
		return .f.
	otherwise
		=creadir(left(w_path,3)+'_bkmemo')
		=creadir(w_path)
		w_path=w_path+'\'
endcase

show get w_path
return
