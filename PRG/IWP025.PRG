*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H 陳陳陳陳陳陳陳陳� Sys.Info 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H Programa        : IWP025.PRG -> Mantenimiento de Perc./Ret. de I. Brutos
*)-H
*)-H Release 		 : 07
*)-H Fecha de inicio : 12/04/2005.-
*)-H Fecha de fin    : 13/04/2005.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*)-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_pref=''
	w_call=.f.
endif

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

w_curobj=_curobj	  &&(Salva numero de objeto en el read activo)
w_alan=alias()		  &&(Salva alias vigente al llamado del programa)
w_wind=wontop()	  &&(Salva ventana vigente al llamado del programa)
w_rdlevel=rdlevel() &&(Salva Nro. de read activo)

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0
w_modicampo=.f.

*) Datos del archivo a mantener------------------------------------------------

w_pfile='25'


*w_clave='m.d25tdgi + m.d25refe + m.d25codi'


w_alias='F'+w_pfile
w_title=db_name(w_alias)
*w_title='Perc./Ret. de Ingresos Brutos'

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F25'

if !w_call

	use (d0f25) in 0 alias 'f25' order 'd25refe' &&(Percep./Ret. Ing. Brutos)
	w_clave		= 'm.d25tdgi + m.d25refe + m.d25cafi + m.d25codi'
	
	if neterr()
		pop key
		pop menu _msysmenu
		fieldvec=''
		if .not. empty(w_wind)
         acti wind (w_wind)
      endif
      =closall()
      return
	endif
&& R.02b Ra�l
ELSE
			SELE f25
			IF (m.d5tico = "7"  AND WONTOP() = 'IWS005C1')				OR				;
				(m.d5tico = "O"  AND WONTOP() = 'IWS005V1')
*					set filter to d25tdgi = w_pref	&& R.01 Mariano
					w_title	= "Percep./Retenc. de Ing. Brutos"
			ELSE
				IF m.d5tico = 'R'
*					set filter to  d25refe = "R" AND d25tdgi = w_pref	&& R.02 Mariano
					w_title	= "Retenciones de Ing. Brutos"
				ELSE
*					set filter to  d25refe = "P" AND d25tdgi = w_pref	&& R.02 Mariano
					w_title	= "Percepciones de Ing. Brutos"
				ENDIF
			ENDIF
&& R.02e Ra�l

	w_orden	=	ORDER('f25')
	SET ORDER TO 'd25refe' IN ('f25')
	w_clave		= 'm.d25codi'
endif

=creatipos('V')	&& perc/ret/otros

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
scatter memvar memo blank

set skip of menu _msysmenu .t.
do ssm999.mpr	&&(menu registro)

=skipbar('registro','insertar',.t.)
if w_call
	=skipbar('registro','Eliminar' ,.t.)
	=skipbar('registro','Modificar',.t.)
endif

on key label f1 do nada
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
endif

SET RELA TO "JD" + d25tdgi		INTO f01t

if w_call

    ***vfp***
*	do brow25
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow25()'
    else
   		do brow25
    endif
    ***vfp***
   	if w_alta
      w_alta=.f.      
      do iws025.spr
   	endif
else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	do iws025.spr
	***vfp***
    if version()='Visual FoxPro'
	    on key label esc
	endif
	***vfp***

endif

pop key
do ssm995.mpr	&& R.07 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu

if .not. empty(w_wind)
	acti wind (w_wind)
endif

if w_call
	SET ORDER TO (w_orden) in ('f25')
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws025
else
	rele wind iws025,barraabm
	=closall()
	=borratipos()
endif
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC Alta25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC Baja25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PRIVATE w_msg
w_msg='Acepta eliminaci�n ?.'

do clav25
if eof(w_alias) .or. .not. st_rlock(w_alias)
	return
endif

if sino(w_msg)='S'
	if begintran()
		=dele_rec(w_alias)
		unlock in (w_alias)
		=endtran()
	endif
	do siguiente
endif
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC clav25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
if w_recn<>0
	w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set order to 'd25refe' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

	if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work25
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
      =work25(.t.)
      w_recn=recno(w_alias)
   endif
   
endif

set orde to (w_orde) in (w_alias)

return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC work25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PARAMETERS w_eof

select (w_alias)
if !w_eof
	scatter memvar memo
else
	scatter memvar memo blank fields except d25tdgi, d25refe, d25cafi, d25codi
endif

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid25(.t.)
	show gets
endif

if .not. eof(w_alias)
	=rlock(w_alias)
   if .not. st_rlock(w_alias,.t.)
   	_curobj=1
   endif
endif
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC when25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D25CODI','m.d25norm')
return(w_retu)

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
FUNC valid25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='25' or (!wexist('IWS025') and wontop()!='IWS025')) and w_show
	if lastkey()=p_esc
		return .t.
	else
		return .f.
	endif
endif
***vfp***

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='D25CAFI' or w_show
	m.d25cafi=strcero(m.d25cafi)
	show get m.d25cafi
endif

if w_vari='D25CODI' or w_show
	m.d25codi=strcero(m.d25codi)
	show get m.d25codi
endif

if w_vari='D25REFE' or w_show
	if !m.d25refe$('PR')
		w_retu=.f.
	endif
	show get m.d25refe
endif

	if w_vari='D25TDGI' .or. w_show
		if !empty(m.d25tdgi) or lastkey()=p_f4
			do fhelp with 'D25TDGI','JD','iwp001t','f01t','clav','desc','Jurisdicci�n DGI Inexistente',(p_strc),'w_retu'
			m.d25tdgi=strcero(m.d25tdgi)
			show get m.d25tdgi
		else
*			m.d25tdgi=strcero(m.d25tdgi)
			w_tdgi=''
			show get m.d25tdgi
			show get w_tdgi
			IF !w_show
						WAIT WIND "DEBE INGRESAR JURISDICCION AFIP"
			ENDIF
			w_retu = .F.
		endif
	endif

&& R.03b Ra�l
*IF w_vari = 'D25CODI'  .OR. w_show								&& R.05 Ra�l
IF (w_vari = 'D25CODI'  .OR. w_show)		AND !EOF()			&& R.05 Ra�l
&& R.02b Ra�l
	w_recno = RECNO()
	LOCATE FOR d25codi = m.d25codi
	IF FOUND()	AND w_recno <> RECNO()
		IF !w_show									&& R.03 Ra�l		
			WAIT WIND 'Ya existe este c�digo definido por el usuario'
		ENDIF										&& R.03 Ra�l
		w_retu	= .F.								&& R.03 Ra�l		&& R.04 Ra�l
	ENDIF			
	IF w_recno <> RECNO()
*			GO w_recn								&& R.03 Ra�l
			GO w_recno								&& R.03 Ra�l
	ENDIF
&& R.02e Ra�l
ENDIF
&& R.03e Ra�l

=ultdepri('D25TDGI','m.d25desc')
=prideult('D25DESC','m.d25desc')
=modicampo('D25TDGI','D25REFE','D25CAFI','D25CODI')

*=ultdepri('D25CODI','m.d25desc')
*=prideult('D25DESC','m.d25desc')
*=modicampo('D25CODI','D25REFE')

return(w_retu)

***vfp***
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
proc actu25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
	on key label f9 do actu25x
	keyboard '{f9}'
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
proc actu25x
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
else
	on key label f9
endif
***vfp***

if !w_agre .and. !w_modi
	return
endif

if !valid25(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav25

if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)

*if wontop() ='IWS005V1'
*if wontop() ='IWS005V1'		OR WONTOP()	='IWS005C1'			&& R.02 Ra�l	&& R.04 Ra�l

*	set order to tag 'd25codi' in (w_alias)						&& R.04 Ra�l
*else															&& R.04 Ra�l
	set order to 'd25refe' in (w_alias)
*endif															&& R.04 Ra�l	
if !seek(w_pref+&w_clave,w_alias)

	if w_agre
    	wait 'Agregando registro...' wind time .4
		if begintran()
	    	=net_appe(w_alias)
	      	sele (w_alias)
	   	   	gather memvar memo
   	   		=endtran()
	   	endif
      	w_modicampo=.f.
      	=db_rlock(w_alias) 
      	if .not. w_call
        	_curobj=1
      	endif
   endif

else 

   if st_rlock(w_alias)
      if w_modi 
         wait 'Modificando registro...' wind time .4
         sele (w_alias)
		 if begintran()
				sele (w_alias)
				gather memvar memo
   	      		=endtran()
   	   	 endif
         w_modicampo=.f.
      endif 
   else 
      do work25(.t.)
   endif
   
endif

w_tdgi = ''				&& R.05 Ra�l

unlock in f98

if w_call
   unlock in (w_alias)
   clear read
endif

set order to (w_orde) in (w_alias)

scatter memvar memo blank
SHOW GETS

return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC brow25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set order to 'd25refe' in (w_alias)

sele (w_alias)
if eof(w_alias)
			go top
endif



if wexist('IWS005V3')	OR wexist('IWS005V1')	OR wexist('IWS005C4')	OR wexist('IWS005C1')	
	browse fields;
		d=f01t.desc	:h='Provincia'	:p='@K' :v=fenter():f	:20, ; 
		d25refe		:h='R�g.'		:p='@K' :v=fenter():f, ; 
		d25cafi		:h='AFIP'		:p='@K' :v=fenter():f, ; 
		d25porc		:h=' % ',;
		d25codi		:h='C�d.'		:p='@K',;
		d25norm		:h='Norma'		:p='@K';
		nome noap noed node norm in screen title (w_title) font 'arial',10;
		valid :f iif(w_call,.t.,salebrow(Lastkey()))
&& R.02b Ra�l
&&						;
&&		for iif(empty(w_pref),.t.,d25tdgi+d25refe=w_pref)
&& R.02e Ra�l

else		
	w_pref=''

	browse fields;
		d=f01t.desc	:h='Provincia'	:p='@K' :v=fenter():f	:20, ; 
		d25refe		:h='R�g.'		:p='@K' :v=fenter():f, ; 
		d25cafi		:h='AFIP'		:p='@K' :v=fenter():f, ; 
		d25porc		:h=' % ',;
		d25codi		:h='C�d.'		:p='@K',;
		d25norm		:h='Norma'		:p='@K';
		nome noap noed node norm in screen title (w_title) font 'arial',10;
		valid :f iif(w_call,.t.,salebrow(Lastkey()))
endif		

*if !w_call											&& R.05 Ra�l
*if !w_call	AND lastkey() <> p_esc					&& R.05 Ra�l  && R.06 Ra�l
&& R.06b  Ra�l
if !w_call
	IF lastkey() <> p_esc			&& R.05 Ra�l
				=work25()
	ELSE
&& R.06e  Ra�l
		=work25(.T.)
	ENDIF							&& R.06b  Ra�l
endif

set order to (w_orde) in (w_alias)
pop menu _msysmenu

***vfp***
IF version()='Visual FoxPro'
	if w_call
		rele wind wbrow
	else
		clear read
		on key label esc do actsalir
	endif
endif
***vfp***

return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC busc25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
do busca
if .not. eof(w_alias)
	do work25
endif
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROC orde25
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�

do case 
   case prompt()='Ordenado por C�digo'
      set orde to 'd25refe' in (w_alias)
   case prompt()='Ordenado por Descripci�n'
      set orde to 'd25norm' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
