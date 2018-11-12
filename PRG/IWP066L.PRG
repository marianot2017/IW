*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : Iwp066l.prg -> Listador de usuarios por empresa
*-H
*-H Fecha de inicio : 
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

if rdlevel()=1
	p_popup=popup()
	p_bar=bar()
endif

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu
set skip of menu _msysmenu .t.

* Apertura de archivos ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
fieldvec[1] = 's01'
fieldvec[2] = 's03'
fieldvec[3] = 'F10'
fieldvec[4] = 'F66'

use ssf001 in 0 alias 's01' order 'd1user'
use ssf003 in 0 alias 's03' order 'd3perf'
use &d0f10 in 0 alias 'f10' order 'p0coem'
use &d0f66 in 0 alias 'f66' 

if neterr()

	pop key
	pop menu _msysmenu
	fieldvec = ''
	=closall()
	return

endif

fieldvec=''

* Variables ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ

w_pfile = '066L'
w_prog  = upper(prompt())
w_alias = 'F66'
w_title = upper(prompt())
w_alta  =.f.

w_dempr  = '00'
w_hempr  = '99'

do ssm998.mpr

p_congtit=.f.

do Iws066l.spr

pop key
pop menu _msysmenu
=closall()
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons66l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons66l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons66l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if !valid066L(.t.)
	wait ' Debe completar todos los datos ! ' wind nowait
	return
endif



use report\Iwr066L.frx in 0 alias freport
sele freport
p_width=width
p_report=dbf()
use


sele f66
set order to d66empr
set rela to d66user into s01

go top


do repdest  && Setea variables internas del reporte

p_repfor='d66empr >= w_dempr .and. d66empr <= w_hempr'

if !empty(p_repclaus)
	p_report='iwr066l'
	=rep_form()
	set relation to
else
	p_repwhile='p_cancela'
endif

do represet && Resetea estado
return

*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function valid066L
*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show
private w_retu, w_vari

if empty(parameters())
	w_show = .f.
endif

w_retu=.t.
w_vari=varread()

if w_vari = 'W_DEMPR' or w_show
	if lastkey()=p_f4
		do fhelp with 'W_DEMPR','','iwp010','f10','p0coem','p0coem','',(p_strc),'w_retu'
	endif
endif

if w_vari = 'W_HEMPR' or w_show
	if lastkey()=p_f4
		do fhelp with 'W_HEMPR','','iwp010','f10','p0coem','p0coem','',(p_strc),'w_retu'
	endif

    if w_hempr < w_dempr
        wait ' Hasta debe ser superior a desde ' wind nowait
        w_retu=.f.
    endif
endif

return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr066l
