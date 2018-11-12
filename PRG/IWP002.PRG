*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP002.PRG -> Mantenimiento de clientes
*)-H
*)-H Release		 : 09
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call

PRIVATE ALL LIKE w_*
if empty(parameters())
	w_call=.f.
	w_pref=''
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

w_pfile='02'
w_clave='m.d2clie'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

w_imco=imco()
w_ultget=iif(w_imco,'D2CUEF','D2INCI')

*) Apertura de archivos--------------------------------------------------------


if !w_call
	fieldvec[1]='F00'
	fieldvec[2]='F01'
	fieldvec[3]='F02'
	fieldvec[4]='F04'
	fieldvec[5]='F13'	&& R.09 Mariano
	fieldvec[6]='F20'
	fieldvec[7]='F23'	&& R.09 Mariano

	use (d0f00) in 0 alias 'f00'						&&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
	use (d0f02) in 0 alias 'f02' order 'd2clie'  &&(Clientes)
	use (d0f04) in 0 alias 'f04' order 'd4peri'  &&(Periodos)
	use (d0f13) in 0 alias 'f13' order 'd13peri' &&(Anticipos y retenciones)	&& R.09 Mariano
	use (d0f20) in 0 alias 'f20' order 'd20tipo' &&(Tipos impos. x periodos)
	use (d0f23) in 0 alias 'f23' order 'd23codi' &&(Alicuotas IVA)	&& R.09 Mariano

	if w_imco		
		ncf01=w0fipre+'01'
		fieldvec[6]='F01c'
		use (ncf01) in 0 alias f01c order tag 'd1cuen'
	endif
	
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
	
	=creatipos('C')
	
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
scatter memvar blank

&& R.08b Mariano
m.d2cuit0=m.d2cuit
m.d2cuit1=m.d2cuit
&& R.08e Mariano

set skip of menu _msysmenu .t.
do iwm002.mpr	&&(menu registro)

=skipbar('registro','insertar',.t.)
if w_call
	=skipbar('registro','Eliminar' ,.t.)
	=skipbar('registro','Modificar',.t.)
endif

on key label f1 do nada
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
endif

if w_call

    ***vfp***
*	do brow02
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow02()'
    else
   		do brow02
    endif
    ***vfp***
   if w_alta
      w_alta=.f.

      =start('99989','f02')
		if !bof('f02')
	      skip -1 in f02
   	   m.d2clie=strcero(str(val(f02->d2clie)+1,5))
		endif
		
      if w_imco
	      do iws002.spr
	   else
	   	do iws002b.spr
	   endif
	endif

else

	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	if w_imco
		do iws002.spr
	else
		do iws002b.spr
	endif
	***vfp***
    if version()='Visual FoxPro'
	    on key label esc
	endif
	***vfp***
	
endif

pop key 
do ssm995.mpr	&& R.04 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu

***vfp***
&& R.03b Mariano
if w_call and version()='Visual FoxPro' and wexist('IWS005V')	&& R.04 Mariano (agregue wexist)
   	on key label esc do actsalir
endif
&& R.03e Mariano
***vfp***

if .not. empty(w_wind)
   acti wind (w_wind)
endif

if w_call
   set orde to 'd2clie' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws002,iws002b
else
	rele wind iws002,iws002b,barraabm
	=closall()
	=borratipos()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC wread02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !w_imco
	show get m.d2cuen disable
	show get m.d2cuec disable
	show get m.d2cuef disable
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav02
if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif

if sino('Acepta eliminaci๓n ?.')='S'
	if begintran()
		=dele_rec(w_alias)
		if seek('C'+m.d2clie,'f20')
			=db_flock('f20')
			sele f20
			delete while d20tipo+d20clie=('C'+m.d2clie) and !eof()
			unlock in f20
			sele (w_alias)
		endif
		unlock in (w_alias)
     	=endtran()
	endif
	do siguiente
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
m.d2clie=strcero(m.d2clie)
show get m.d2clie

if w_recn<>0
   w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd2clie' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work02
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
		w_oclav=&w_clave
      do work02
      w_recn=recno(w_alias)
      &w_clave=w_oclav      
   	show get &w_clave
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
select (w_alias)
scatter memvar

&& R.08b Mariano
m.d2cuit0=m.d2cuit
m.d2cuit1=m.d2cuit
&& R.08e Mariano

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid02(.t.)	
	show gets
endif

if .not. eof(w_alias)
   =rlock(w_alias)
   if .not. st_rlock(w_alias,.t.) 
   	_curobj=1
   endif
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC when02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

if !eof('f02')	&& No es alta
	if w_vari='D2TIPO'
		=valid02(.f.)
		w_retu=.f.
	endif
	=irclave('D2CLIE','m.d2nomb')
else
	=irclave('D2CLIE','m.d2tipo')
endif

*if !m.d2tido$'80.86'	&& fuerzo a que muestre el get activo
*	show get m.d2cuit	&& ojo!!, hay dos gets en el screen con esta variable encimados
*endif

***vfp***
&& R.08b Mariano
*if version()='Visual FoxPro'
*	if w_vari='D2CUIT'	and !p_actsalir	&& R.05 Mariano (agregue p_actsalir)
*		keyb '{end}'+'{home}'+'{shift+rightarrow}'+'{shift+end}'
*	endif
*endif
if w_retu 
	if w_vari='D2CUIT0'
		if !m.d2tido$'80.86.87'
			w_retu=.f.
			if at('-',m.d2cuit0)>0
				m.d2cuit0=strtran(m.d2cuit0,'-','')+'  '	
				m.d2cuit1=m.d2cuit0
			endif
		else
			if at('-',m.d2cuit0)=0
				m.d2cuit0=left(m.d2cuit0,2)+'-'+substr(m.d2cuit0,3,8)+'-'+substr(m.d2cuit0,11,1)
				m.d2cuit1=m.d2cuit0
			endif
		endif
	endif
	if w_vari='D2CUIT1' and m.d2tido$'80.86.87'
		if !empty(m.d2cuit1)
			w_retu=.f.
			m.d2cuit1=strtran(m.d2cuit1,'-','')
			m.d2cuit0=left(m.d2cuit1,2)+'-'+substr(m.d2cuit1,3,8)+'-'+substr(m.d2cuit1,11,1)
			m.d2cuit1=m.d2cuit0
		endif
	endif
	if w_retu
		if !m.d2tido$'80.86.87'
			if w_vari='D2CUIT0' and !(lastkey()=p_enter or lastkey()=p_tab or lastkey()=p_right or lastkey()=p_down)
				_curobj=objnum(m.d2cuit1)
				w_retu=.f.
			endif
		else
			if w_vari='D2CUIT1' and !(lastkey()=p_enter or lastkey()=p_tab or lastkey()=p_right or lastkey()=p_down)
				_curobj=objnum(m.d2cuit0)
				w_retu=.f.
			endif
		endif
		if w_vari='D2CUIT0' and !p_actsalir
			keyb '{end}{home}{shift+rightarrow}{shift+end}'
		endif
	else
		if w_vari='D2CUIT1' and !(lastkey()=p_enter or lastkey()=p_tab or lastkey()=p_right or lastkey()=p_down)
			_curobj=objnum(m.d2cuit0)
		endif
	endif
endif
&& R.08e Mariano
***vfp***

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='02' or (wontop()!='IWS002' and wontop()!='IWS002B')) and w_show	&& R.03 Mariano ( !wexist('IWS002') and )
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

IF EMPTY(m.d2tido)
			m.d2tido	= "80"
			SHOW GETS
ENDIF

if w_vari='D2TIPO' or w_show
	do fhelp with 'D2TIPO','TI','iwp001t','f01t','clav','desc','Tipo impositivo Inexistente',(p_char),'w_retu'
endif

if w_vari='D2NOMB' or w_show
	if empty(m.d2nomb)
		if !w_show
			wait wind 'Debe especificar el nombre del cliente...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D2TIDO' or w_show
	do fhelp with 'D2TIDO','TD','iwp001t','f01t','clav','desc','Tipo de documento Inexistente',(p_char),'w_retu'
	&& R.02b Mariano
*	IF !m.d2tido$p_tdoc	
*		RETURN .F.
*	ENDIF
	&& R.02e Mariano

*?????????????????????????'''
	if m.d2tido$'80.86.87'
		if empty(strtran(m.d2cuit0,'-',''))	&& R.01 Ra๚l Valida CDI		&& R.08 Mariano (d2cuit0 x d2cuit)
		      m.d2cuit0='  -        - '		&& R.08 Mariano (d2cuit0 x d2cuit)
		      show get m.d2cuit0	&& R.08 Mariano (d2cuit0 x d2cuit)
		endif
	endif
	&& R.08b Mariano
	if !empty(m.d2cuit)
		if m.d2tido$'80.86.87'
			m.d2cuit0=strtran(m.d2cuit0,'-','')
			m.d2cuit0=left(m.d2cuit0,2)+'-'+substr(m.d2cuit0,3,8)+'-'+substr(m.d2cuit0,11,1)
			m.d2cuit1=m.d2cuit0
			m.d2cuit=m.d2cuit0
			show get m.d2cuit1
		else
			m.d2cuit1=strtran(m.d2cuit1,'-','')
			m.d2cuit0=m.d2cuit1
			m.d2cuit=m.d2cuit1
			show get m.d2cuit1
		endif
	endif
	if version()='Visual FoxPro'
		keyboard '{end}{home}'
	endif
	&& R.08e Mariano
endif

&& R.08b Mariano
if w_vari='D2CUIT0'
	m.d2cuit=m.d2cuit0
	m.d2cuit1=m.d2cuit0
endif
if w_vari='D2CUIT1'
	m.d2cuit=m.d2cuit1
	m.d2cuit0=m.d2cuit1
endif
&& R.08e Mariano

if w_vari='D2CUIT' or w_show
		   if .not. eof('f02')
		    		  w_re02=recno('f02')
		   else
			   w_re02=0
		   endif
			DO CASE
				CASE m.d2tido = "99"
						m.d2cuit1 = SPACE(13)	&& R.08 Mariano (d2cuit1 x d2cuit)

*				Para Operaciones con el exterior
*				CASE m.d2tido = "80" AND (LEFT(m.d2cuit,2) = "50" OR LEFT(m.d2cuit,2) = "55")

				CASE m.d2tido$'80.86.87'	&& R.01 Ra๚l Valida CDI
						if (DiCuit(left(m.d2cuit0,11))<>right(m.d2cuit0,1)) .and. !empty(strtran(m.d2cuit0,'-',''))	&& R.08 Mariano (d2cuit0 x d2cuit)
								if !w_show
										wait window 'Error en CUIT o CUIL o CDI'	&& R.01 Ra๚l Valida CDI
								endif
								w_retu=.f.
						ENDIF
		   				IF EMPTY(m.d2cuit0) 	&& R.08 Mariano (d2cuit0 x d2cuit)
    		  						IF !w_show
											WAIT WIND "DEBE INGRESAR N๚mero de documento "
											w_retu = .F.
									ENDIF
		   				ENDIF
					    w_ord2=order(w_alias)
					    set order to d2cuit in f02
		   				IF (seek(m.d2cuit0,'f02') and m.d2clie<>f02.d2clie)	&& R.08 Mariano (d2cuit0 x d2cuit)
	  		  						if .not. w_show
									        	wait 'Advertencia: N๚mero de documento '+m.d2cuit0+' REPETIDO. Cliente : '+f02.d2clie wind
*												w_retu = .F.
									endif
		   				endif
		   				set order to (w_ord2) in f02
		   				if .not. empty(w_re02)
    		  							go w_re02 in f02
		   				else
    		 				=db_goeof('f02')
		   				endif
			ENDCASE		
			&& R.08b Mariano
			if !w_retu and version()='Visual FoxPro'
				keyboard '{end}{home}'
			endif
			&& R.08e Mariano
endif

if w_imco and (w_vari='D2CUEN' or w_show)
	&& R.06b Mariano
	if empty(m.d2cuen) and lastkey()!=p_f4	&& R.07 Mariano (agregue and lastkey()!=p_f4)
		w_cuen=''
		show get w_cuen
	else
	&& R.06e Mariano
		do fhelp with 'D2CUEN','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
   		if !f01c->d1hijo=0
		   	wait wind 'Cuenta no imputable...'
		   	w_retu=.f.
		endif
	endif	&& R.06 Mariano
endif

if w_imco and (w_vari='D2CUEC' or w_show)
	&& R.06b Mariano
	if empty(m.d2cuec) and lastkey()!=p_f4	&& R.07 Mariano (agregue and lastkey()!=p_f4)
		w_cuec=''
		show get w_cuec
	else
	&& R.06e Mariano
		do fhelp with 'D2CUEC','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
		if !f01c->d1hijo=0
		   wait wind 'Cuenta no imputable...'
		   w_retu=.f.
		endif
	endif	&& R.06 Mariano
endif

if w_imco and (w_vari='D2CUEF' or w_show)
	&& R.06b Mariano
	if empty(m.d2cuef) and lastkey()!=p_f4	&& R.07 Mariano (agregue and lastkey()!=p_f4)
		w_cuef=''
		show get w_cuef
	else
	&& R.06e Mariano
		do fhelp with 'D2CUEF','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
		if !f01c->d1hijo=0
		   	wait wind 'Cuenta no imputable...'
		   	w_retu=.f.
		endif
	endif	&& R.06 Mariano
endif

if w_vari='D2JURI' or w_show
	do fhelp with 'D2JURI','JU','iwp001','f01','d1cla','d1des','Jurisdicci๓n inexistente',(p_char),'w_retu'
endif

&& R.09b Mariano
if w_vari='D2RUBR' or w_show
	if !empty(m.d2rubr) or lastkey()=p_f4
		do fhelp with 'D2RUBR','RV','iwp001','f01','d1cla','d1des','Rubro inexistente',(p_strc),'w_retu'
	else
		w_rubr=''
		show get w_rubr
	endif
endif
&& R.09e Mariano

if w_vari='D2INCI' or w_show
	if !m.d2inci$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N...'
		endif
		w_retu=.f.
	endif
endif

select(w_alias)	&& R.08 Mariano

=ultdepri('D2CLIE','m.'+w_ultget)
=prideult(w_ultget,'m.'+w_ultget)
=modicampo('D2CLIE')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu02x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu02x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
else
	on key label f9
endif
***vfp***

if !w_agre .and. !w_modi
    return
endif

if !valid02(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav02
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)

w_orde=order(w_alias)
set orde to 'd2clie' in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
      wait 'Agregando registro...' wind time .4
		if begintran()
	      =net_appe(w_alias)
	      sele (w_alias)
   	   gather memvar 
	  	   =vactu02()
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
				gather memvar
				=vactu02()
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work02
   endif
   
endif

unlock in f97

if w_call
   unlock in (w_alias)
   clear read
endif

set order to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC vactu02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !seek('C'+m.d2clie,'f20')
	go bott in f04
   if !eof('f04')
   	*) Tomo ultimo periodo cargado
   	w20peri=f04->d4peri
   else
      *) o lo construyo desde la fecha
      w20peri=strcero(str(month(date()),2))+'/'+str(year(date()),4)
   endif
   =net_appe('f20')
   sele f20
   repl f20->d20tipo with 'C',;
        f20->d20clie with m.d2clie,;
        f20->d20peri with w20peri,;
        f20->d20tiim with m.d2tipo
   unlock in 'f20'
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set orde to 'd2nomb' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)

browse fields ;
	d2clie :h='C๓digo':p='@K':v=fenter():f,;
	d2nomb :h='Nombre' ,;
	d2cuit :h='C.U.I.T.' ,;
	coim=coim(d2tipo) :h='Cond. Impositiva' ;
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work02()
endif

set order to (w_orde) in (w_alias)
pop menu _msysmenu

***vfp***
IF version()='Visual FoxPro'
	if w_call
		rele wind wbrow
		if wexist('IWS005V')	&& R.04 Mariano
			on key label esc do actsalir
		endif	&& R.04 Mariano
	else
		clear read
		on key label esc do actsalir
	endif
endif
***vfp***

return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC busc02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca with '','','3-Por C.U.I.T.'
if .not. eof(w_alias)
   do work02
endif
set orde to w_orde in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde02
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd2clie' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd2nomb' in (w_alias)
   case prompt()='Ordenado por cUit'
     	set orde to 'd2cuit' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
