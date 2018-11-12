*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : Iwp066.prg -> Mantenimiento Usuarios/Empresas
*-H
*-H Release 		: 01 
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

if rdlevel()=1
	p_popup=popup()
	p_bar=bar()
endif

** Salvado de estados anteriores 

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

w_curobj=_curobj     && Salva numero de objeto en el read activo
w_alan=alias()       && Salva alias vigente al llamado del programa 
w_wind=wontop()      && Salva ventana vigente al llamado del programa 
w_rdlevel=rdlevel()  && Salva Nro. de read activo
*w_Use =.F.
w_modicampo=.f.

** Variables internas
w_prog=upper(prompt())
w_alta=.f.
w_agre=.t.
w_modi=.t.
w_recn=0

** Datos del archivo a mantener
w_pfile='66'
w_clave='m.D66USER+m.D66EMPR'
w_alias='F'+w_pfile
w_title=db_name(w_alias)

if ! w_call

   Fieldvec[1] = 'S01'
   Fieldvec[2] = 'S03'
   Fieldvec[3] = 'F10'
   Fieldvec[4] = 'F66'
     
   use ssf001 in 0 alias s01 order d1user      && Usuarios
   use ssf003 in 0 alias s03 order d3perf      && Perfiles
   use &d0f10 in 0 alias f10 order p0coem      && Empresas
   use &d0f66 in 0 alias f66 order d66user     && Usuarios / Empresas
      
   if neterr()

      pop key  
      pop menu _msysmenu

      if .not. empty(w_wind)
         acti wind (w_wind)
      endif
	  fieldvec=''
      =closall()
      return
   endif
endif

sele &w_alias
set relation to d66user into s01
set relation to d66empr into f10 addi

if .not. w_call
   =db_goeof(w_alias)
endif
scatte memvar blank

set skip of menu _msysmenu .t.

do ssm999.mpr && Menu General
=skipbar('Registro','Insertar',.t.)

On Key label F1 Do nada
if !skipbarstat('registro','agregar')
	On Key Label F9 keyb '{ctrl+f9}'
endif

if w_call
   do brow66
   if w_alta
      w_alta=.f. 
      scatter memvar blank 
   
      =skipbar('Registro','Modificar' ,.t.)
      =skipbar('Registro','Eliminar'  ,.t.)
      
      do iws066.spr   

   endif
else 
   if !empty(w_wind)
      deac wind (w_wind)
   endif   

   do iws066.spr   

endif

pop key 
do ssm995.mpr	&& R.01 Mariano (puesto porque en vfp quedan activas los key del menu usado y de esta manera se limpian)
pop menu _msysmenu

if w_call
    set orde to 1 in (w_alias)
    if .not. empty(w_alan)
        sele &w_alan
    endif
    if .not. empty(w_wind)
       acti wind (w_wind)
    endif
	relea wind iws066
else
	relea wind barraabm,iws066
    =closall()
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC Alta66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

if w_call
   w_alta=.t.
   deac wind (w_title)
endif
 
return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC Baja66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do clav66 

if eof(w_alias) .or. .not. st_rlock(w_alias)
   return 
endif


if sino('จ Acepta eliminacion ? ')='S'
  =dele_rec(w_alias)
  unlock in (w_alias)
  do siguiente 
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC clav66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)

*&w_clave=strcero(&w_clave)
show get m.d66user
show get m.d66empr

if w_recn<>0
   w_recn=recno(w_alias)
endif

set orde to 1 in (w_alias)

if seek(w_pref+&w_clave,w_alias) 

   if w_recn<>recno(w_alias) .or. .not. st_rlock(w_alias)
      do work66 
      w_recn=recno(w_alias)
   endif

   if w_call
      unlock in (w_alias)
      clear read
   endif

else 

   w_cuser=m.d66user
   w_cempr=m.d66empr
   w_duser=w_user
   w_dempr=w_empr

   if w_recn<>recno(w_alias)
      do work66 
      w_recn=recno(w_alias)
   endif

   m.d66user=w_cuser
   m.d66empr=w_cempr
   w_user   =w_duser
   w_empr   =w_dempr

   show gets 

endif   

set orde to w_orde in (w_alias)

return 
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC when66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
w_retu=.t.
w_vari=varread()
p_curobj=_curobj
=irclave('D66USER','m.d66empr')
return w_retu

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC valid66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETER w_show

if parameters()=0 
   w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

if w_vari='D66USER' .or. w_show
   do fhelp with 'D66USER','','ssp001','s01','d1user' ,'d1nomb', 'Usuario inexistente',(p_char),'w_retu'
endif

=ultdepri('D66USER','m.d66empr')

if w_vari='D66EMPR' .or. w_show
    do fhelp with 'D66EMPR','','iwp010','f10','p0coem' ,'p0empr', 'Empresa inexistente',(p_strc),'w_retu'
endif   

=prideult('D66EMPR','m.d66empr')

=modicampo('D66USER','D66EMPR')

return w_retu

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu66
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do actu66x
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc actu66x
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
If !w_agre .and. !w_modi
   Return
Endif
   
do clav66

if .not. eof(w_alias) .and. .not. st_rlock(w_alias)
   return 
endif   

if .not. valid66(.t.)
   wait ' Debe completar todos los datos ! ' wind nowait
   return 
endif 

=db_capt(w_alias)


w_orde=order(w_alias)
set orde to 1 in (w_alias)

if .not. seek(w_pref+&w_clave,w_alias) 

   if w_agre
      wait ' Agregando registro... ' wind time .4
      =net_appe(w_alias)
      gather memvar 
      =db_rlock(w_alias) 
      w_modicampo=.f.
      if .not. w_call
         _curobj=1
      endif
  endif
else 
   if st_rlock(w_alias) 
      if w_modi 
         wait ' Modificando registro... ' wind time .4
         gather memvar
         w_modicampo=.f.
      endif 
   else 
      do work66
   endif      
endif 

unlock in f98

set order to (w_orde) in (w_alias)
if w_call
   unlock in (w_alias)
   clear read
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC work66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

scatter memvar 

if eof(w_alias)
*    store .t. to m.d1tal1,m.d1tal2,m.d1tal3,m.d1tal4,m.d1tal5,m.d1tal6,m.d1tal7
endif

if w_rdlevel<>rdlevel()
     show gets 
     =valid66(.t.)
endif 

if .not. eof(w_alias)
   =rlock(w_alias)
   if .not. st_rlock(w_alias,.t.) 
      _curobj=1
   endif
endif  

return .t.

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC brow66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

activate screen 

w_orde=order(w_alias)

push menu _msysmenu


=skipbar('Registro','Insertar'  ,!w_call)

=skipbar('Registro','Eliminar'  ,.t.)
=skipbar('Registro','Consultar' ,.t.)
=skipbar('Registro','Agregar'   ,.t.)
=skipbar('Registro','Elegir'    ,.t.)
=skipbar('Registro','Modificar' ,.t.)

set orde to 2 in (w_alias)

if eof(w_alias)
    go top in (w_alias)
endif

defi wind w_most from 0,0 to 15,79 color scheme 10

browse  ;
   fields s01.d1nomb :h='Usuario':v=fenter() :f ,;
          d66empr :h='Empresa' ,;
          f10.p0empr :h='Descripci๓n' ;
     title w_title ;
     nomenu ;
     noedit ;
     nodelete ;
     valid :f salebrow(Lastkey()) ;
     font 'Arial', 10 ;
     noappend ;
     window w_most
     
rele wind w_most

set orde to w_orde in (w_alias)

do work66

pop menu _msysmenu

return 

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC busc66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

w_orde=order(w_alias)
set orde to 1 in (w_alias) 
do busca
if .not. eof(w_alias)
   do work66
endif
set orde to w_orde in (w_alias)
return 

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC Orde66
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

do case 
   case prompt()='Por Usuario'
      set orde to 1 in (w_alias)
   case prompt()='Por Empresa'
      set orde to 2 in (w_alias)
endcase

if wexist(w_title)
   show wind (w_title) refresh
endif

return

*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
    PROC BrowUser
*-R อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
activate screen 

push menu _msysmenu


=skipbar('Registro','Insertar'  ,.f.)

=skipbar('Registro','Eliminar'  ,.t.)
=skipbar('Registro','Elegir'    ,.t.)
=skipbar('Registro','Agregar'   ,.t.)
=skipbar('Registro','Elegir'    ,.t.)
=skipbar('Registro','Modificar' ,.t.)

set orde to 1 in f01

if eof(w_alias)
    go top in f01
endif

browse  ;
   fields f01.d1user :h='Usuario':v=fenter() :f ,;
          f01.d1nomb :h='Nombre' ;
     title w_title ;
     nomenu ;
     noedit ;
     valid :f salebrow(Lastkey()) ;
     nodelete ;
     noappend 

pop menu _msysmenu

return