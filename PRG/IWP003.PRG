*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP003.PRG -> Mantenimiento de proveedores
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

w_pfile='03'
w_clave='m.d3prov'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

w_imco=imco()
w_ultget=iif(w_imco,'D3CUEF','D3DAGP')

*) Apertura de archivos--------------------------------------------------------

if !w_call

	fieldvec[1]='F00'
	fieldvec[2]='F01'
	fieldvec[3]='F03'
	fieldvec[4]='F04'
	fieldvec[5]='F13'	&& R.09 Mariano
	fieldvec[6]='F20'
	fieldvec[7]='F23'	&& R.09 Mariano

	use (d0f00) in 0 alias 'f00'						&&(Parametros)
	use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
	use (d0f03) in 0 alias 'f03' order 'd3prov'  &&(Proveedores)
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
	
	=creatipos('P')
	
endif

fieldvec=''

*)-----------------------------------------------------------------------------

select (w_alias)
scatter memvar blank

&& R.08b Mariano
m.d3cuit0=m.d3cuit
m.d3cuit1=m.d3cuit
&& R.08e Mariano

set skip of menu _msysmenu .t.
do iwm003.mpr	&&(menu registro)

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
*	do brow03
    if version()='Visual FoxPro'
		*** cambio el lastkey() ***
		keyboard '{tab}'
		=inkey()
		DO wbrow WITH 'brow03()'
    else
   		do brow03
    endif
    ***vfp***
   if w_alta
      w_alta=.f.

      =start('99989','f03')
		if !bof('f03')
	      skip -1 in f03
   	   m.d3prov=strcero(str(val(f03->d3prov)+1,5))
   	endif   

      if w_imco
      	do iws003.spr
      else
      	do iws003b.spr
      endif
   endif

else
	
	***vfp***
    if version()='Visual FoxPro'
	    on key label esc do actsalir
	endif
	***vfp***
	if w_imco
	   	do iws003.spr
	else
	   	do iws003b.spr
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
if w_call and version()='Visual FoxPro'	and wexist('IWS005C')	&& R.04 Mariano
   	on key label esc do actsalir
endif
&& R.03e Mariano
***vfp***

if .not. empty(w_wind)
   acti wind (w_wind)
endif

if w_call
   set orde to 'd3prov' in (w_alias)
   if .not. empty(w_alan)
   	sele (w_alan)
   	w_alias=w_alan
   endif
   rele wind iws003,iws003b
else
	rele wind iws003,iws003b,barraabm
	=closall()
	=borratipos()
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC wread03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !w_imco
	show get m.d3cuen disable
	show get m.d3cuec disable
	show get m.d3cuef disable
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if w_call
	w_alta=.t.
   deac wind (w_title)
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do clav03
if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif

if sino('Acepta eliminaci๓n ?.')='S'
	if begintran()
		=dele_rec(w_alias)
		if seek('P'+m.d3prov,'f20')
			=db_flock('f20')
			sele f20
			delete while d20tipo+d20clie=('P'+m.d3prov) and !eof()
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
PROC clav03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
m.d3prov=strcero(m.d3prov)
show get m.d3prov

if w_recn<>0
   w_recn=recno(w_alias)
endif

w_orde=order(w_alias)
set orde to 'd3prov' in (w_alias)

if seek(w_pref+&w_clave,w_alias)

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work03
      w_recn=recno(w_alias)
   endif
   if w_call
      unlock in (w_alias)
      clear read
   endif

else

	if w_recn<>recno(w_alias)
		w_oclav=&w_clave
      do work03
      w_recn=recno(w_alias)
      &w_clave=w_oclav      
   	show get &w_clave
   endif
   
endif

set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
select (w_alias)
scatter memvar

&& R.08b Mariano
m.d3cuit0=m.d3cuit
m.d3cuit1=m.d3cuit
&& R.08e Mariano

if w_rdlevel<>rdlevel() and iif(version()='Visual Fox', !w_call,.t.)	&& ***vfp***
	=valid03(.t.)	
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
PROC when03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj

if !eof('f03')	&& No es alta
	if w_vari='D3TIPO'
		=valid03(.f.)
		w_retu=.f.
	endif
	=irclave('D3PROV','m.d3nomb')
else
	=irclave('D3PROV','m.d3tipo')
endif

*if !m.d3tido$'80.86'	&& fuerzo a que muestre el get activo
*	show get m.d3cuit	&& ojo!!, hay dos gets en el screen con esta variable encimados
*endif

***vfp***
&& R.08b Mariano
*if version()='Visual FoxPro'
*	if w_vari='D3CUIT'	and !p_actsalir	&& R.05 Mariano (agregue p_actsalir)
*		keyb '{end}'+'{home}'+'{shift+rightarrow}'+'{shift+end}'
*	endif
*endif
if w_retu 
	if w_vari='D3CUIT0'
		if !m.d3tido$'80.86.87'
			w_retu=.f.
			if at('-',m.d3cuit0)>0
				m.d3cuit0=strtran(m.d3cuit0,'-','')+'  '	
				m.d3cuit1=m.d3cuit0
			endif
		else
			if at('-',m.d3cuit0)=0
				m.d3cuit0=left(m.d3cuit0,2)+'-'+substr(m.d3cuit0,3,8)+'-'+substr(m.d3cuit0,11,1)
				m.d3cuit1=m.d3cuit0
			endif
		endif
	endif
	if w_vari='D3CUIT1' and m.d3tido$'80.86.87'
		if !empty(m.d3cuit1)
			w_retu=.f.
			m.d3cuit1=strtran(m.d3cuit1,'-','')
			m.d3cuit0=left(m.d3cuit1,2)+'-'+substr(m.d3cuit1,3,8)+'-'+substr(m.d3cuit1,11,1)
			m.d3cuit1=m.d3cuit0
		endif
	endif
	if w_retu
		if !m.d3tido$'80.86.87'
			if w_vari='D3CUIT0' and !(lastkey()=p_enter or lastkey()=p_tab or lastkey()=p_right or lastkey()=p_down)
				_curobj=objnum(m.d3cuit1)
				w_retu=.f.
			endif
		else
			if w_vari='D3CUIT1' and !(lastkey()=p_enter or lastkey()=p_tab or lastkey()=p_right or lastkey()=p_down)
				_curobj=objnum(m.d3cuit0)
				w_retu=.f.
			endif
		endif
		if w_vari='D3CUIT0' and !p_actsalir
			keyb '{end}{home}{shift+rightarrow}{shift+end}'
		endif
	else
		if w_vari='D3CUIT1' and !(lastkey()=p_enter or lastkey()=p_tab or lastkey()=p_right or lastkey()=p_down)
			_curobj=objnum(m.d3cuit0)
		endif
	endif
endif
&& R.08e Mariano
***vfp***

return(w_retu)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC valid03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

***vfp***
if version()='Visual Fox' and (w_pfile!='03' or (wontop()!='IWS003' and wontop()!='IWS003B')) and w_show	&& R.03 Mariano (!wexist('IW003') and )
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

IF EMPTY(m.d3tido)
			m.d3tido = "80"
			SHOW GETS
ENDIF

if w_vari='D3TIPO' or w_show
	do fhelp with 'D3TIPO','TI','iwp001t','f01t','clav','desc','Tipo impositivo Inexistente',(p_char),'w_retu'
endif

if w_vari='D3NOMB' or w_show
	if empty(m.d3nomb)
		if !w_show
			wait wind 'Debe especificar el nombre del proveedor...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D3TIDO' or w_show
	do fhelp with 'D3TIDO','TD','iwp001t','f01t','clav','desc','Tipo de documento Inexistente',(p_char),'w_retu'
	&& R.02b Mariano
*	IF !m.d3tido$p_tdoc
*		RETURN .F.
*	ENDIF
	&& R.02e Mariano
	if m.d3tido$'80.86.87' .and. empty(strtran(m.d3cuit0,'-',''))	&& R.01 Ra๚l Valida CDI	&& R.08 Mariano (d3cuit0 x d3cuit)
		m.d3cuit0='  -        - '	&& R.08 Mariano (d3cuit0 x d3cuit)
		show get m.d3cuit0		&& R.08 Mariano (d3cuit0 x d3cuit)
	endif
	&& R.08b Mariano
	if !empty(m.d3cuit)
		if m.d3tido$'80.86.87'
			m.d3cuit0=strtran(m.d3cuit0,'-','')
			m.d3cuit0=left(m.d3cuit0,2)+'-'+substr(m.d3cuit0,3,8)+'-'+substr(m.d3cuit0,11,1)
			m.d3cuit1=m.d3cuit0
			m.d3cuit=m.d3cuit0
			show get m.d3cuit1
		else
			m.d3cuit1=strtran(m.d3cuit1,'-','')
			m.d3cuit0=m.d3cuit1
			m.d3cuit=m.d3cuit1
			show get m.d3cuit1
		endif
	endif
	if version()='Visual FoxPro'
		keyboard '{end}{home}'
	endif
	&& R.08e Mariano
endif

&& R.08b Mariano
if w_vari='D3CUIT0'
	m.d3cuit=m.d3cuit0
	m.d3cuit1=m.d3cuit0
endif
if w_vari='D3CUIT1'
	m.d3cuit=m.d3cuit1
	m.d3cuit0=m.d3cuit1
endif
&& R.08e Mariano

if w_vari='D3CUIT' or w_show
		   if .not. eof('f03')
		      w_re03=recno('f03')
		   else
	    	  w_re03=0
		   endif
			DO CASE
				CASE m.d3tido = "99"
						m.d3cuit1 = SPACE(13)		&& R.08 Mariano (d3cuit1 x d3cuit)

*				Para Operaciones con el exterior
				CASE m.d3tido = "80" AND (LEFT(m.d3cuit,2) = "50" OR LEFT(m.d3cuit,2) = "55")

				CASE m.d3tido$'80.86.87'	&& R.01 Ra๚l Valida CDI
						if (DiCuit(left(m.d3cuit0,11))<>right(m.d3cuit0,1)) .and. !empty(strtran(m.d3cuit0,'-',''))	&& R.08 Mariano (d3cuit0 x d3cuit)
								if !w_show
										wait window 'Error en CUIT o CUIL o CDI'	&& R.01 Ra๚l Valida CDI
								endif
								w_retu=.f.
						ENDIF
		   				IF EMPTY(m.d3cuit0) 	&& R.08 Mariano (d3cuit0 x d3cuit)
    		  						IF !w_show
											WAIT WIND "DEBE INGRESAR N๚mero de documento "
													w_retu = .F.
									ENDIF
		   				ENDIF
						w_ord3=order(w_alias)
						set order to d3cuit in f03
						if seek(m.d3cuit0,'f03') and m.d3prov<>f03.d3prov	&& R.08 Mariano (d3cuit0 x d3cuit)
									if .not. w_show
												wait 'Advertencia: N๚mero de documento REPETIDO. Proveedor : '+f03.d3prov wind
*												w_retu = .F.
									endif
						endif
						set order to (w_ord3) in f03
						if .not. empty(w_re03)
										go w_re03 in f03
						else
							=db_goeof('f03')
						endif
			ENDCASE
			&& R.08b Mariano
			if !w_retu and version()='Visual FoxPro'
				keyboard '{end}{home}'
			endif
			&& R.08e Mariano
endif

if w_imco and (w_vari='D3CUEN' or w_show)
	&& R.06b Mariano
	if empty(m.d3cuen) and lastkey()!=p_f4	&& R.07 Mariano (agregue and lastkey()!=p_f4)
		w_cuen=''
		show get w_cuen
	else
	&& R.06e Mariano
		do fhelp with 'D3CUEN','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
		if !f01c->d1hijo=0
	   		wait wind 'Cuenta no imputable...'
		   	w_retu=.f.
		endif
	endif	&& R.06e Mariano
endif

if w_imco and (w_vari='D3CUEC' or w_show)
	&& R.06b Mariano
	if empty(m.d3cuec) and lastkey()!=p_f4	&& R.07 Mariano (agregue and lastkey()!=p_f4)
		w_cuec=''
		show get w_cuec
	else
	&& R.06e Mariano
		do fhelp with 'D3CUEC','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
		if !f01c->d1hijo=0
		   	wait wind 'Cuenta no imputable...'
		   	w_retu=.f.
		endif
	endif	&& R.06 Mariano
endif

if w_imco and (w_vari='D3CUEF' or w_show)
	&& R.06b Mariano
	if empty(m.d3cuef) and lastkey()!=p_f4	&& R.07 Mariano (agregue and lastkey()!=p_f4)
		w_cuef=''
		show get w_cuef
	else
	&& R.06e Mariano
		do fhelp with 'D3CUEF','','cgp001c','f01c','d1cuen','d1desc','Cuenta Inexistente',(p_char),'w_retu'
		if !f01c->d1hijo=0
		   	wait wind 'Cuenta no imputable...'
		   	w_retu=.f.
		endif
	endif	&& R.06 Mariano
endif

if w_vari='D3INCI' or w_show
	if !m.d3inci$'SN'
		if !w_show
			wait wind 'Opciones vแlidas: S,N...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='D3JURI' or w_show
	do fhelp with 'D3JURI','JU','iwp001','f01','d1cla','d1des','Jurisdicci๓n inexistente.',(p_char),'w_retu'
endif

&& R.09b Mariano
if w_vari='D3RUBR' or w_show
	if !empty(m.d3rubr) or lastkey()=p_f4
		do fhelp with 'D3RUBR','RC','iwp001','f01','d1cla','d1des','Rubro inexistente',(p_strc),'w_retu'
	else
		w_rubr=''
		show get w_rubr
	endif
endif
&& R.09e Mariano

select(w_alias)	&& R.08 Mariano

=ultdepri('D3PROV','m.'+w_ultget)
=prideult(w_ultget,'m.'+w_ultget)
=modicampo('D3PROV')
return(w_retu)

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu03x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu03x
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

if !valid03(.t.)
   wait 'Debe completar datos...' wind
	return
endif

do clav03
if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif

=db_capt(w_alias)
w_orde=order(w_alias)
set orde to 'd3prov' in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias)

	if w_agre
      wait 'Agregando registro...' wind time .4
		if begintran()
	      =net_appe(w_alias)
	      sele (w_alias)
   	   gather memvar 
   	   =vactu03()
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
				=vactu03()
   	      =endtran()
   	   endif
         w_modicampo=.f.
      endif 
   else 
      do work03
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
PROC vactu03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !seek('P'+m.d3prov,'f20')
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
   repl f20->d20tipo with 'P',;
        f20->d20clie with m.d3prov,;
        f20->d20peri with w20peri,;
        f20->d20tiim with m.d3tipo
   unlock in 'f20'
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
push menu _msysmenu
=skipbar('Registro','Insertar' ,!w_call)
=skipbar('Registro','Agregar'	 ,.t.)
=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Modificar',.t.)
=skipbar('Registro','Consultar',.t.)
=skipbar('Registro','Elegir'	 ,.t.)

w_orde=order(w_alias)
set orde to 'd3nomb' in (w_alias)
if eof(w_alias)
	go top in (w_alias)
endif

sele (w_alias)
browse fields ;
	d3prov :h='C๓digo':p='@K':v=fenter():f,;
	d3nomb :h='Nombre' ,;
	d3cuit :h='C.U.I.T.' ,;
	coim=coim(d3tipo) :h='Cond. Impositiva' ;
	nome noap noed node norm in screen title (w_title) font 'arial',10;
	valid :f iif(w_call,.t.,salebrow(Lastkey()))

if !w_call
	=work03()
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
PROC busc03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_orde=order(w_alias)
do busca with '','','3-Por C.U.I.T.'
if .not. eof(w_alias)
	do work03
endif
set orde to (w_orde) in (w_alias)
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC orde03
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 'd3prov' in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
     	set orde to 'd3nomb' in (w_alias)
   case prompt()='Ordenado por cUit'
     	set orde to 'd3cuit' in (w_alias)
endcase
if wexist(w_title)
	show wind (w_title) refresh
endif
return
