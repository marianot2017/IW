*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H 陳陳陳陳陳陳陳陳� Sys.Info 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H Programa        : iwp022d.prg -> Depura Auditoria
*-H
*-H Fecha de inicio : 
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
*-H 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様
PRIVATE all like w_*

** Salvado de estados anteriores 

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

set skip of menu _msysmenu .t.

** Apertura de archivos **

Fieldvec[1] = 'F10'
Fieldvec[2] = 'F22'

use &d0f10 in 0 alias f10 order p0coem  		&& Empresas
use &d0f22 in 0 alias f22 order d22empr         && Auditoria

if neterr()
    pop key  
    pop menu _msysmenu

    =closall()
    return
endif

w_pfile = '22d'

w_prog=upper(prompt())

w_alias = 'F22'
w_dbname= db_name(w_alias)

sele (w_alias)

w_alta=.f.

do SSM998.mpr

w_dempr='00'
w_dfech=d22fech

w_hempr='99'
w_hfech=date()

do iws022d.spr
 
pop key  
pop menu _msysmenu
        
=closall()

return

* 様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
Function Valid22d
*様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PARAMETERS w_show

w_retu=.t.
w_vari=varread()

if w_vari='W_DEMPR' 
  
    sele f10
	
	If Lastkey() = p_f4
		Select F10
        do fhelp with 'W_DEMPR','','iwp010','f10','p0coem' ,'p0empr', 'Empresa inexistente',(p_char),'w_retu'
   	    if lastkey() <> 27
   	       w_dempr=F10.p0coem
   	    endif
   	Endif

    If Empty(w_dempr)
       w_dempr = '00'
    Endif
    
    Show Get w_dempr
Endif

if w_vari='W_HEMPR' 
  
    sele f10
	
	If Lastkey() = p_f4
		Select F10
        do fhelp with 'W_HEMPR','','iwp010','f10','p0coem' ,'p0empr', 'Empresa inexistente',(p_char),'w_retu'
   	    if lastkey() <> 27
   	       w_hempr=F10.p0coem
   	    endif
   	Endif

    If Empty(w_hempr)
       w_hempr = '99'
    Endif
    
    Show Get w_hempr
Endif
    
if w_vari='W_DFECH' or w_show
		IF !TRDATE('w_dfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
			ENDIF
			w_retu	= .F.
			_curobj	= objnum(w_dfech)
			RETURN .F.
		ENDIF
endif

if w_vari='W_HFECH' or w_show
		IF !TRDATE('w_hfech')
			IF !w_show
					WAIT WIND "La fecha no es correcta"
			ENDIF
			w_retu	= .F.
			_curobj	= objnum(w_hfech)
			RETURN .F.
		ENDIF
		if w_dfech>w_hfech
				if !w_show
						wait wind 'Hasta debe ser mayor o igual que desde...'
				endif
				w_retu=.f.
		endif
endif

if w_hempr < w_dempr or w_hfech < w_dfech
    wait ' Hasta debe ser superior a desde ' wind nowait
    w_retu=.f.
 endif
    
return w_retu

***vfp***
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
proc cons22d
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
	on key label f9 do cons22d0
	keyboard '{f9}'
return

*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
proc cons22d0
*)様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
on key label f9
***vfp***


   if sino(' Confirma la depuraci�n de Auditor�a ? ', 'NO', 'Depurar Auditor�a')='S'

      if !lockemp('')
         =ErrorNet(075)
         return
      endif

      sele f77
      use

      use &d0f77 in 0 alias f77 excl
      if neterr()
         =ErrorNet(076)
         =login('S')
         =login('E',.t.)
         =unlockemp('')

         return
      endif
      
      *Depuracion

      sele f22
	  use &d0f22 alias f22 order d22empr exclusive       && Auditoria

	  if neterr()
		  sele f77
		  use
         =ErrorNet(076)
         =login('S')
         =login('E',.t.)
         =unlockemp('')

		return
	  endif


      dele for d22empr>=w_dempr and d22empr<=w_hempr and ;
               d22fech>=w_dfech and d22fech<=w_hfech 

      wait window 'Fin de la Depuraci�n' timeout 0.5

      =unlockemp('')

	  sele f77
	  use

	  =login('S')
	  =login('E',.t.)

   endif

return
