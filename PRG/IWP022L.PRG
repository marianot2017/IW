*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : iwp022l.prg -> Listado de Auditoria
*-H
*-H Fecha de inicio : 
*-H Fecha de fin    :
*-H
*-H Updates
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE all like w_*

** Salvado de estados anteriores 

push key clear      && Salva estados de teclas 
push menu _msysmenu && Salva estado de menu

set skip of menu _msysmenu .t.

** Apertura de archivos **

Fieldvec[1] = 'F10'
Fieldvec[2] = 'F22'

use &d0f10 in 0 alias f10 order p0coem        && Empresas
use &d0f22 in 0 alias f22 order d22empr       && Auditoria

if neterr()
    pop key  
    pop menu _msysmenu

    =closall()
    return
endif

w_pfile = '22l'

w_prog=upper(prompt())

w_alias = 'F22'
w_dbname= db_name(w_alias)

sele (w_alias)

w_alta=.f.
do ssm998.mpr

use report\iwr022l.frx in 0 alias freport         && Reporte
sele freport
p_width=width
p_report=dbf()
use        
p_congtit=.t.

w_orde=1

sele (w_alias)
go top

w_dempr='00'
w_dfech=d22fech

w_hempr='99'
if dtos(w_dfech)<=dtos(date())
	w_hfech=date()
else
	w_hfech=w_dfech+1
endif

do iws022l.spr
 
pop key  
pop menu _msysmenu
        
=closall()

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons22l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons22l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons22l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid22l(.t.)
	wait ' Debe completar todos los datos ! ' wind nowait
	return 
endif

sele f10
set order to tag p0coem

sele (w_alias)
set relation to d22empr into f10

do repdest  && Setea variables internas del reporte

if !empty(p_repclaus)	&& debe ser preview o to printer

	p_report = 'iwr022l'

	p_repfor='w_dempr <= d22empr and d22empr <= w_hempr and w_dfech <= d22fech and d22fech <= w_hfech'
	
	=rep_form()
					
	p_report=''

else	&& to file
	
	p_repfields=''
	
	p_repfor='w_dempr <= d22empr and d22empr <= w_hempr and w_dfech <= d22fech and d22fech <= w_hfech'
	
	p_repwhile='p_cancela'

endif

do represet && Resetea estado 

return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Function Valid22l
*อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show
PRIVATE w_retu,w_vari

w_retu=.t.
w_vari=varread()

if w_vari='W_DEMPR' 
  
 	if lastkey()=p_f4
	    do fhelp with 'W_DEMPR','','iwp010','f10','p0coem' ,'p0empr', 'Empresa inexistente',(p_strc),'w_retu'
	else
		w_dempr=strcero(w_dempr)
		show get w_dempr
	endif		

Endif

if w_vari='W_HEMPR' 
  
	if lastkey()=p_f4
	    do fhelp with 'W_HEMPR','','iwp010','f10','p0coem' ,'p0empr', 'Empresa inexistente',(p_strc),'w_retu'
	else
		w_hempr=strcero(w_hempr)
		show get w_hempr
	endif	   

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

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
Func AuTiOp
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
priv au_tiope,au_desde,au_hasta

au_tiope=''
au_desde=''
au_hasta=''

do case
   case f22->d22tiop='ALT'
        au_tiope='Alta Comprobante'
        au_desde='De:'

   case f22->d22tiop='MOD'
        au_tiope='Modificaci๓n Comprobante'
        au_desde='De:'

   case f22->d22tiop='BAJ'
        au_tiope='Baja Comprobante'
        au_desde='De:'

   case f22->d22tiop='ACT'
        au_tiope='Activa Auditoria'
        au_desde='Desde:'

   case f22->d22tiop='DES'
        au_tiope='Desactiva Auditoria'
        au_desde='Desde:'

   case f22->d22tiop='RCE'
        au_tiope='Recupero Empresa'
        au_desde='Result๓:'

   case f22->d22tiop='RSE'
        au_tiope='Resguardo Empresa'

   case f22->d22tiop='MPE'
        au_tiope='Modifica Perํodo'
        au_desde='Ventas:'
        au_hasta='Compras:'

   case f22->d22tiop='APE'
        au_tiope='Alta Perํodo'
        au_hasta=''

   case f22->d22tiop='BPE'
        au_tiope='Baja Perํodo'
        au_hasta=''

   case f22->d22tiop='TRM'
        au_tiope='Generaci๓n Archivo'
        au_desde='Desde:'
        au_hasta='Hasta:'

   case f22->d22tiop='INM'
        au_tiope='Incorporaci๓n Archivo'
        au_desde='Desde:'
        au_hasta='Hasta:'

   case f22->d22tiop='DIN'
        au_tiope='Deshacer Ult.Incorp.'
        au_desde='Desde:'
        au_hasta='Hasta:'

   case f22->d22tiop='BAM'
        au_tiope='Baja Masiva'
        au_desde='Desde:'
        au_hasta='Hasta:'

   case f22->d22tiop='ALE'
        au_tiope='Alta Empresa'

   case f22->d22tiop='BAE'
        au_tiope='Baja Empresa'

   case f22->d22tiop='IVV'
        au_tiope='IVA Ventas MemoSoft'
        au_desde='Gener๓ Asiento:'
        au_hasta='Del Perํodo:'

   case f22->d22tiop='IVC'
        au_tiope='IVA Compras MemoSoft'
        au_desde='Gener๓ Asiento:'
        au_hasta='Del Perํodo:'

   case f22->d22tiop='TOM'
        au_tiope='Toma de Empresa DOS'
        au_desde='Result๓:'

   other

        au_tiope='XXXXXXXXXXXXXX'
        au_desde='XXXXXXXXXXXXXX'
        au_hasta='XXXXXXXXXXXXXX'

endcase

au_tiope=left (au_tiope+spac(30),30)
au_desde=right(spac(15)+au_desde,15)
au_hasta=right(spac(15)+au_hasta,15)

return au_tiope+' '+au_desde+' '+f22->d22lede+' '+au_hasta+' '+f22->d22leha

***puesto adrede para que incluya el report al proyecto**
report form iwr022l
