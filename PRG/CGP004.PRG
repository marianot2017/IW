*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : Cgp004.prg -> Mantenimiento de Subdiarios
*-H
*-H Fecha de inicio : 
*-H Fecha de fin    :
*-H
*-H Actualizaciones
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_pref,w_call
PRIVATE ALL LIKE w_*
   
if parameters()=0
   w_call=.f.
   w_pref=''
endif

** Salvado de estados anteriores 

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

w_curobj = _curobj     && Salva numero de objeto en el read activo
w_alan   = alias()       && Salva alias vigente al llamado del programa 
w_wind   = wontop()      && Salva ventana vigente al llamado del programa 
w_rdlevel=rdlevel()  && Salva Nro. de read activo

** Variables internas

w_prog=upper(prompt())

w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0

** Datos del archivo a mantener

w_pfile='04'
w_clave='m.D4TIPO'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

w_modicampo=.f.	&& variable que indica si se modifico un campo del abm

if .not. w_call

   Fieldvec[1] = 'f04'
   
   use &d0f04 in 0 alias f04 order d4tipo exclusive    && Subdiarios
   
   if neterr()

      pop key  
      pop menu _msysmenu

      if .not. empty(w_wind)
         acti wind (w_wind)
      endif
      
      Fieldvec = ''
      =closall()
      return

   endif
   
endif

Fieldvec = ''


sele &w_alias

if .not. w_call
   =db_goeof(w_alias)
endif

scatte memvar blank
set skip of menu _msysmenu .t.

do ssm999.mpr && Menu Ayuda

if set('DEVELOP')='ON'
    do window.mpr
endif    

if !w_call
   =skipbar('Registro','Insertar' ,.t.)
endif

On Key Label F1 Do Nada

if w_agre
	On Key Label F9 keyboard '{ctrl+f9}'
endif

if w_call
   do brow04
   if w_alta
      w_alta=.f. 
      scatter memvar blank 

	  =skipbar('Registro','Insertar',.t.)
      =skipbar('Registro','Eliminar'  ,.t.)
      =skipbar('Registro','Modificar' ,.t.)

      do cgs004.spr   

   endif
else 
   =skipbar('Registro','Elegir',.t.)
   do cgs004.spr   

endif

pop key 
pop menu _msysmenu

if .not. empty(w_wind)
   acti wind (w_wind)
endif

if w_call
    set orde to 1 in (w_alias)
    if .not. empty(w_alan)
        sele &w_alan
    endif
    
    release window cgs004
    
else

	release window cgs004, barraabm
	
    =closall()
endif

return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Alta04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if w_call
   w_alta=.t.
   deac wind (w_title)
endif
 
return
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Baja04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do clav04 

if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif
if m.d4tipo='0'
	werr1='No se puede ELIMINAR este subdiario'
	=advsuave(werr1)
	return
endif

if 'P'$m.d4cier .or. 'C'$m.d4cier
    wait 'Existen pases cargados, imposible eliminar' wind TIME .4
    return
endif

if d0esej='C'
    wait 'Ejercicio cerrado, imposible eliminar' wind TIME .4
    return
endif

if sino('Acepta eliminaci๓n ? ')='S'

	if begintran()

		=dele_rec(w_alias)
		unlock in (w_alias)
		
		sele f04
		scatter memvar blank
		w_de01=''
		w_de02=''
		w_de03=''
		w_de04=''
		w_de05=''
		w_de06=''
		w_de07=''
		w_de08=''
		w_de09=''
		w_de10=''
		w_de11=''
		w_de12=''
		_curobj=1
		show gets
		
		=endtran()
		
	endif
	
endif

return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC clav04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)

&w_clave=strcero(&w_clave)
show get &w_clave

if w_recn<>0
   w_recn=recno(w_alias)
endif

set orde to 1 in (w_alias)

if seek(w_pref+&w_clave,w_alias) 

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work04 
      w_recn=recno(w_alias)
   endif

   if w_call
      unlock in (w_alias)
      clear read
   endif

else 

   w_cclav=&w_clave

   if w_recn<>recno(w_alias)
      do work04 
      w_recn=recno(w_alias)
   endif

   &w_clave=w_cclav   

   show gets 

endif   

set orde to w_orde in (w_alias)

return 

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC when04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	w_retu=.t.
	w_vari=varread()

	p_curobj=_curobj
	
	=irclave('D4TIPO', 'm.d4desc')

return w_retu

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETER w_show

if parameters()=0 
   w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

*===> Validaciones, antes no validaba

IF w_vari = "D4DESC"	.OR. w_show
	IF EMPTY(m.d4desc)
		IF !w_show
			WAIT WIND "La descripci๓n no puede estar en blanco"
		ENDIF
		IF w_retu
				_curobj	= 2
				w_retu = .F.
		ENDIF
	ENDIF
ENDIF			

*===>

for w_cont=1 to 12
    w_desc='Sin movimientos'
    w_esta=substr(m.d4cier,w_cont,1)
    if w_esta='P'
        if m.d4tipo<>'0'
            w_desc='Cierre pendiente'
        else
            w_desc='Con Movimientos '
        endif
    else
        if w_esta='C'
            w_desc='Cerrado         '
        endif
    endif
    
    w_macr=strcero(str(w_cont,2))
    w_de&w_macr=w_desc
    
    show get w_de&w_macr
    
next

*if w_vari='D4ASIE' .or. w_show
*    m.d4asie=strcero(m.d4asie)
*    show get m.d4asie
*endif

*===> Agregado del IF para que si hay errores quede posicionado en el campo	
	IF w_retu
			=ultdepri('D4TIPO', 'm.d4desc')
			=prideult('D4DESC','m.d4desc')
	ENDIF
*===>

=modicampo('D4TIPO')

return w_retu
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Actu04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)

if !w_agre .and. !w_modi
   Return
Endif
   
do clav04

if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif   

if .not. valid04(.t.)
   wait ' Debe completar todos los datos ! ' wind nowait
   return 
endif 

if d0esej='C'
    wait 'Ejercicio cerrado' wind nowait
    return
endif

if m.d4tipo='0'
*    return
endif

=db_capt(w_alias)

set orde to 1 in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias) 

   if w_agre
	
		if begintran()
		
		      wait  ' Agregando registro... ' wind time .4 
    		  =net_appe(w_alias)
	    	  gather memvar 

			  w_modicampo=.f.	&& variable que indica si se modifico un campo del abm

		      =db_rlock(w_alias) 
	      
	    	  =endtran()
	      
		endif

  endif
else 
   if st_rlock(w_alias) 
      if w_modi 

			if begintran()
			
		         wait ' Modificando registro... ' wind time .4 
        		 gather memvar

				 w_modicampo=.f.	&& variable que indica si se modifico un campo del abm
				 
				 =endtran()
				 
			endif

      endif 
   else 
      do work04
   endif      
endif 

unlock in f98


if w_call
   unlock in (w_alias)
   clear read
else
   _curobj=1
endif

set orde to w_orde in (w_alias)

return
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

scatter memvar 

if eof(w_alias)
*    store .t. to m.d1tal1,m.d1tal2,m.d1tal3,m.d1tal4,m.d1tal5,m.d1tal6,m.d1tal7
endif

if w_rdlevel<>rdlevel()
     show gets 
     =valid04(.t.)
endif 

if .not. eof(w_alias)
   =rlock(w_alias)
   if .not. st_rlock(w_alias,.t.) 
      _curobj=1
   endif
endif  

return .t.

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC brow04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
activate screen 

w_orde=order(w_alias)

push menu _msysmenu

if .not. w_call
   =skipbar('Registro','Insertar' ,.t.)
endif

=skipbar('Registro','Eliminar' ,.t.)
=skipbar('Registro','Elegir'    ,.t.)
=skipbar('Registro','Consultar'  ,.t.)
=skipbar('Registro','Modificar' ,.t.)
=skipbar('Registro','Agregar' ,.t.)
=skipbar('Registro','Comentarios' ,.t.)

set orde to 2 in (w_alias)

if eof(w_alias)
    go top in (w_alias)
endif

browse  ;
   fields d4tipo :h='Tipo':v=fenter() :f ,; 
          d4desc :h='Descripci๓n' ;
     title w_title ;
     nomenu ;
     noedit ;
     nodelete ;
     valid :f salebrow(Lastkey()) ;
     font 'Arial', 10 ;
     noappend 


set orde to w_orde in (w_alias)

do work04

pop menu _msysmenu

return 
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC busc04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)
do busca

if .not. eof(w_alias)
   do work04
endif

set orde to w_orde in (w_alias)
return 
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC Orde04
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do case 
   case prompt()='Ordenado por C๓digo'
      set orde to 1 in (w_alias)
   case prompt()='Ordenado por Descripci๓n'
      set orde to 2 in (w_alias)
endcase

if wexist(w_title)
   show wind (w_title) refresh
endif

return
*******************************************************************
