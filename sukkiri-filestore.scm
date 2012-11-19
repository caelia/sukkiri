;;; sukkiri-filestore.scm -- A file storage extension for Sukkiri. Using
;;;   this extension, you can interact with content stored in files as
;;;   if they were part of the database.
;;; Copyright © 2012 by Matthew C. Gushee <matt@gushee.net>
;;; This is open source software, released under the BSD license. See
;;;   the accompanying LICENSE file for details.

(module sukkiri-filestore
        *
        (import scheme)
        (import chicken)
        (import posix)
        (import files)
        (import ports)
        (import extras)
        (import srfi-13)
        (import data-structures)


;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  CONFIGURATION PARAMETERS  ------------------------------------------

(define *file-storage-path* (make-parameter #f))

(define *version-control* (make-parameter 'git))

(define *use-version-control* (make-parameter #t))

(define *sukkiri-fs-debug* (make-parameter #f))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  UTILITY FUNCTIONS  -------------------------------------------------

(define (eprintf fmt . args)
  (error (apply sprintf (cons fmt args))))

(define (debug-msg . msgs)
  (when (*sukkiri-fs-debug*)
    (with-output-to-port
      (current-error-port)
      (lambda () (apply print msgs)))))

(define (flip-pair pair)
  (unless (pair? pair) (error "Not a pair."))
  (cons (cdr pair) (car pair)))

(define (zpad nstr #!optional (len 4))
  (let loop ((nstr* nstr))
    (if (>= (string-length nstr*) len)
      nstr*
      (loop (string-append "0" nstr*)))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  MEDIA TYPES  -------------------------------------------------------

;; This is currently a toy implementation
(define *imt-types*
  (make-parameter
    '((application . #x1000)
      (audio . #x2000)
      (image . #x3000)
      (message . #x4000)
      (model . #x5000)
      (multipart . #x6000)
      (text . #x7000)
      (video . #x8000))))

(define *imt-types-reverse*
  (make-parameter
    (map flip-pair (*imt-types*))))

(define *imt-application-types*
  (make-parameter
    '((1d-interleaved-parityfec . #x0001) (3gpp-ims+xml . #x0002) (activemessage . #x0003)
      (andrew-inset . #x0004) (applefile . #x0005) (atom+xml . #x0006)
      (atomdeleted+xml . #x0007) (atomicmail . #x0008) (atomcat+xml . #x0009)
      (atomsvc+xml . #x000a) (auth-policy+xml . #x000b) (batch-SMTP . #x000c)
      (beep+xml . #x000d) (calendar+xml . #x000e) (cals-1840 . #x000f)
      (ccmp+xml . #x0010) (ccxml+xml . #x0011) (cdmi-capability . #x0012)
      (cdmi-container . #x0013) (cdmi-domain . #x0014) (cdmi-object . #x0015)
      (cdmi-queue . #x0016) (cea-2018+xml . #x0017) (cellml+xml . #x0018)
      (cfw . #x0019) (cnrp+xml . #x001a) (commonground . #x001b)
      (conference-info+xml . #x001c) (cpl+xml . #x001d) (csta+xml . #x001e)
      (CSTAdata+xml . #x001f) (cybercash . #x0020) (davmount+xml . #x0021)
      (dca-rft . #x0022) (dec-dx . #x0023) (dialog-info+xml . #x0024)
      (dicom . #x0025) (dns . #x0026) (dskpp+xml . #x0027)
      (dssc+der . #x0028) (dssc+xml . #x0029) (dvcs . #x002a)
      (ecmascript . #x002b) (EDI-Consent . #x002c) (EDIFACT . #x002d)
      (EDI-X12 . #x002e) (emma+xml . #x002f) (epp+xml . #x0030)
      (eshop . #x0031) (exi . #x0032) (fastinfoset . #x0033)
      (fastsoap . #x0034) (fdt+xml . #x0035) (fits . #x0036)
      (font-tdpfr . #x0037) (framework-attributes+xml . #x0038) (gzip . #x0039)
      (H224 . #x003a) (held+xml . #x003b) (http . #x003c)
      (hyperstudio . #x003d) (ibe-key-request+xml . #x003e) (ibe-pkg-reply+xml . #x003f)
      (ibe-pp-data . #x0040) (iges . #x0041) (im-iscomposing+xml . #x0042)
      (index . #x0043) (index.cmd . #x0044) (index.obj . #x0045)
      (index.response . #x0046) (index.vnd . #x0047) (inkml+xml . #x0048)
      (iotp . #x0049) (ipfix . #x004a) (ipp . #x004b)
      (isup . #x004c) (javascript . #x004d) (json . #x004e)
      (kpml-request+xml . #x004f) (kpml-response+xml . #x0050) (link-format . #x0051)
      (lost+xml . #x0052) (lostsync+xml . #x0053) (mac-binhex40 . #x0054)
      (macwriteii . #x0055) (mads+xml . #x0056) (marc . #x0057)
      (marcxml+xml . #x0058) (mathematica . #x0059) (mathml-content+xml . #x005a)
      (mathml-presentation+xml . #x005b) (mathml+xml . #x005c) (mbms-associated-procedure-description+xml . #x005d)
      (mbms-deregister+xml . #x005e) (mbms-envelope+xml . #x005f) (mbms-msk-response+xml . #x0060)
      (mbms-msk+xml . #x0061) (mbms-protection-description+xml . #x0062) (mbms-reception-report+xml . #x0063)
      (mbms-register-response+xml . #x0064) (mbms-register+xml . #x0065) (mbms-user-service-description+xml . #x0066)
      (mbox . #x0067) (media_control+xml . #x0068) (media-policy-dataset+xml . #x0069)
      (mediaservercontrol+xml . #x006a) (metalink4+xml . #x006b) (mets+xml . #x006c)
      (mikey . #x006d) (mods+xml . #x006e) (moss-keys . #x006f)
      (moss-signature . #x0070) (mosskey-data . #x0071) (mosskey-request . #x0072)
      (mp21 . #x0073) (mp4 . #x0074) (mpeg4-generic . #x0075)
      (mpeg4-iod . #x0076) (mpeg4-iod-xmt . #x0077) (msc-ivr+xml . #x0078)
      (msc-mixer+xml . #x0079) (msword . #x007a) (mxf . #x007b)
      (nasdata . #x007c) (news-checkgroups . #x007d) (news-groupinfo . #x007e)
      (news-transmission . #x007f) (nlsml+xml . #x0080) (nss . #x0081)
      (ocsp-request . #x0082) (ocsp-response . #x0083) (octet-stream . #x0084)
      (oda . #x0085) (oebps-package+xml . #x0086) (ogg . #x0087)
      (oxps . #x0088) (parityfec . #x0089) (patch-ops-error+xml . #x008a)
      (pdf . #x008b) (pgp-encrypted . #x008c) (pgp-keys . #x008d)
      (pgp-signature . #x008e) (pidf+xml . #x008f) (pidf-diff+xml . #x0090)
      (pkcs10 . #x0091) (pkcs7-mime . #x0092) (pkcs7-signature . #x0093)
      (pkcs8 . #x0094) (pkix-attr-cert . #x0095) (pkix-cert . #x0096)
      (pkixcmp . #x0097) (pkix-crl . #x0098) (pkix-pkipath . #x0099)
      (pls+xml . #x009a) (poc-settings+xml . #x009b) (postscript . #x009c)
      (prs.alvestrand.titrax-sheet . #x009d) (prs.cww . #x009e) (prs.nprend . #x009f)
      (prs.plucker . #x00a0) (prs.rdf-xml-crypt . #x00a1) (prs.xsf+xml . #x00a2)
      (pskc+xml . #x00a3) (rdf+xml . #x00a4) (qsig . #x00a5)
      (raptorfec . #x00a6) (reginfo+xml . #x00a7) (relax-ng-compact-syntax . #x00a8)
      (remote-printing . #x00a9) (resource-lists-diff+xml . #x00aa) (resource-lists+xml . #x00ab)
      (riscos . #x00ac) (rlmi+xml . #x00ad) (rls-services+xml . #x00ae)
      (rpki-ghostbusters . #x00af) (rpki-manifest . #x00b0) (rpki-roa . #x00b1)
      (rpki-updown . #x00b2) (rtf . #x00b3) (rtx . #x00b4)
      (samlassertion+xml . #x00b5) (samlmetadata+xml . #x00b6) (sbml+xml . #x00b7)
      (scvp-cv-request . #x00b8) (scvp-cv-response . #x00b9) (scvp-vp-request . #x00ba)
      (scvp-vp-response . #x00bb) (sdp . #x00bc) (sep+xml . #x00bd)
      (set-payment . #x00be) (set-payment-initiation . #x00bf) (set-registration . #x00c0)
      (set-registration-initiation . #x00c1) (sgml . #x00c2) (sgml-open-catalog . #x00c3)
      (shf+xml . #x00c4) (sieve . #x00c5) (simple-filter+xml . #x00c6)
      (simple-message-summary . #x00c7) (simpleSymbolContainer . #x00c8) (slate . #x00c9)
      (smil . #x00ca) (smil+xml . #x00cb) (smpte336m . #x00cc)
      (soap+fastinfoset . #x00cd) (soap+xml . #x00ce) (sparql-query . #x00cf)
      (sparql-results+xml . #x00d0) (spirits-event+xml . #x00d1) (srgs . #x00d2)
      (srgs+xml . #x00d3) (sru+xml . #x00d4) (ssml+xml . #x00d5)
      (tamp-apex-update . #x00d6) (tamp-apex-update-confirm . #x00d7) (tamp-community-update . #x00d8)
      (tamp-community-update-confirm . #x00d9) (tamp-error . #x00da) (tamp-sequence-adjust . #x00db)
      (tamp-sequence-adjust-confirm . #x00dc) (tamp-status-query . #x00dd) (tamp-status-response . #x00de)
      (tamp-update . #x00df) (tamp-update-confirm . #x00e0) (tei+xml . #x00e1)
      (thraud+xml . #x00e2) (timestamp-query . #x00e3) (timestamp-reply . #x00e4)
      (timestamped-data . #x00e5) (tve-trigger . #x00e6) (ulpfec . #x00e7)
      (vcard+xml . #x00e8) (vemmi . #x00e9) (vnd.3gpp.bsf+xml . #x00ea)
      (vnd.3gpp.pic-bw-large . #x00eb) (vnd.3gpp.pic-bw-small . #x00ec) (vnd.3gpp.pic-bw-var . #x00ed)
      (vnd.3gpp.sms . #x00ee) (vnd.3gpp2.bcmcsinfo+xml . #x00ef) (vnd.3gpp2.sms . #x00f0)
      (vnd.3gpp2.tcap . #x00f1) (vnd.3M.Post-it-Notes . #x00f2) (vnd.accpac.simply.aso . #x00f3)
      (vnd.accpac.simply.imp . #x00f4) (vnd.acucobol . #x00f5) (vnd.acucorp . #x00f6)
      (vnd.adobe.formscentral.fcdt . #x00f7) (vnd.adobe.fxp . #x00f8) (vnd.adobe.partial-upload . #x00f9)
      (vnd.adobe.xdp+xml . #x00fa) (vnd.adobe.xfdf . #x00fb) (vnd.aether.imp . #x00fc)
      (vnd.ah-barcode . #x00fd) (vnd.ahead.space . #x00fe) (vnd.airzip.filesecure.azf . #x00ff)
      (vnd.airzip.filesecure.azs . #x0100) (vnd.americandynamics.acc . #x0101) (vnd.amiga.ami . #x0102)
      (vnd.amundsen.maze+xml . #x0103) (vnd.anser-web-certificate-issue-initiation . #x0104) (vnd.antix.game-component . #x0105)
      (vnd.apple.mpegurl . #x0106) (vnd.apple.installer+xml . #x0107) (vnd.arastra.swi . #x0108)
      (vnd.aristanetworks.swi . #x0109) (vnd.astraea-software.iota . #x010a) (vnd.audiograph . #x010b)
      (vnd.autopackage . #x010c) (vnd.avistar+xml . #x010d) (vnd.balsamiq.bmml+xml . #x010e)
      (vnd.blueice.multipass . #x010f) (vnd.bluetooth.ep.oob . #x0110) (vnd.bmi . #x0111)
      (vnd.businessobjects . #x0112) (vnd.cab-jscript . #x0113) (vnd.canon-cpdl . #x0114)
      (vnd.canon-lips . #x0115) (vnd.cendio.thinlinc.clientconf . #x0116) (vnd.century-systems.tcp_stream . #x0117)
      (vnd.chemdraw+xml . #x0118) (vnd.chipnuts.karaoke-mmd . #x0119) (vnd.cinderella . #x011a)
      (vnd.cirpack.isdn-ext . #x011b) (vnd.claymore . #x011c) (vnd.cloanto.rp9 . #x011d)
      (vnd.clonk.c4group . #x011e) (vnd.cluetrust.cartomobile-config . #x011f) (vnd.cluetrust.cartomobile-config-pkg . #x0120)
      (vnd.collection+json . #x0121) (vnd.collection.next+json . #x0122) (vnd.commerce-battelle . #x0123)
      (vnd.commonspace . #x0124) (vnd.cosmocaller . #x0125) (vnd.contact.cmsg . #x0126)
      (vnd.crick.clicker . #x0127) (vnd.crick.clicker.keyboard . #x0128) (vnd.crick.clicker.palette . #x0129)
      (vnd.crick.clicker.template . #x012a) (vnd.crick.clicker.wordbank . #x012b) (vnd.criticaltools.wbs+xml . #x012c)
      (vnd.ctc-posml . #x012d) (vnd.ctct.ws+xml . #x012e) (vnd.cups-pdf . #x012f)
      (vnd.cups-postscript . #x0130) (vnd.cups-ppd . #x0131) (vnd.cups-raster . #x0132)
      (vnd.cups-raw . #x0133) (vnd.curl . #x0134) (vnd.cyan.dean.root+xml . #x0135)
      (vnd.cybank . #x0136) (vnd.dart . #x0137) (vnd.data-vision.rdz . #x0138)
      (vnd.dece.data . #x0139) (vnd.dece.ttml+xml . #x013a) (vnd.dece.unspecified . #x013b)
      (vnd.dece.zip . #x013c) (vnd.denovo.fcselayout-link . #x013d) (vnd.dir-bi.plate-dl-nosuffix . #x013e)
      (vnd.dm.delegation+xml . #x013f) (vnd.dna . #x0140) (vnd.dolby.mobile.1 . #x0141)
      (vnd.dolby.mobile.2 . #x0142) (vnd.dpgraph . #x0143) (vnd.dreamfactory . #x0144)
      (vnd.dtg.local . #x0145) (vnd.dtg.local.flash . #x0146) (vnd.dtg.local.html . #x0147)
      (vnd.dvb.ait . #x0148) (vnd.dvb.dvbj . #x0149) (vnd.dvb.esgcontainer . #x014a)
      (vnd.dvb.ipdcdftnotifaccess . #x014b) (vnd.dvb.ipdcesgaccess . #x014c) (vnd.dvb.ipdcesgaccess2 . #x014d)
      (vnd.dvb.ipdcesgpdd . #x014e) (vnd.dvb.ipdcroaming . #x014f) (vnd.dvb.iptv.alfec-base . #x0150)
      (vnd.dvb.iptv.alfec-enhancement . #x0151) (vnd.dvb.notif-aggregate-root+xml . #x0152) (vnd.dvb.notif-container+xml . #x0153)
      (vnd.dvb.notif-generic+xml . #x0154) (vnd.dvb.notif-ia-msglist+xml . #x0155) (vnd.dvb.notif-ia-registration-request+xml . #x0156)
      (vnd.dvb.notif-ia-registration-response+xml . #x0157) (vnd.dvb.notif-init+xml . #x0158) (vnd.dvb.pfr . #x0159)
      (vnd.dvb.service . #x015a) (vnd.dxr . #x015b) (vnd.dynageo . #x015c)
      (vnd.easykaraoke.cdgdownload . #x015d) (vnd.ecdis-update . #x015e) (vnd.ecowin.chart . #x015f)
      (vnd.ecowin.filerequest . #x0160) (vnd.ecowin.fileupdate . #x0161) (vnd.ecowin.series . #x0162)
      (vnd.ecowin.seriesrequest . #x0163) (vnd.ecowin.seriesupdate . #x0164) (vnd.emclient.accessrequest+xml . #x0165)
      (vnd.enliven . #x0166) (vnd.eprints.data+xml . #x0167) (vnd.epson.esf . #x0168)
      (vnd.epson.msf . #x0169) (vnd.epson.quickanime . #x016a) (vnd.epson.salt . #x016b)
      (vnd.epson.ssf . #x016c) (vnd.ericsson.quickcall . #x016d) (vnd.eszigno3+xml . #x016e)
      (vnd.etsi.aoc+xml . #x016f) (vnd.etsi.cug+xml . #x0170) (vnd.etsi.iptvcommand+xml . #x0171)
      (vnd.etsi.iptvdiscovery+xml . #x0172) (vnd.etsi.iptvprofile+xml . #x0173) (vnd.etsi.iptvsad-bc+xml . #x0174)
      (vnd.etsi.iptvsad-cod+xml . #x0175) (vnd.etsi.iptvsad-npvr+xml . #x0176) (vnd.etsi.iptvservice+xml . #x0177)
      (vnd.etsi.iptvsync+xml . #x0178) (vnd.etsi.iptvueprofile+xml . #x0179) (vnd.etsi.mcid+xml . #x017a)
      (vnd.etsi.overload-control-policy-dataset+xml . #x017b) (vnd.etsi.sci+xml . #x017c) (vnd.etsi.simservs+xml . #x017d)
      (vnd.etsi.tsl+xml . #x017e) (vnd.etsi.tsl.der . #x017f) (vnd.eudora.data . #x0180)
      (vnd.ezpix-album . #x0181) (vnd.ezpix-package . #x0182) (vnd.f-secure.mobile . #x0183)
      (vnd.fdf . #x0184) (vnd.fdsn.mseed . #x0185) (vnd.fdsn.seed . #x0186)
      (vnd.ffsns . #x0187) (vnd.fints . #x0188) (vnd.FloGraphIt . #x0189)
      (vnd.fluxtime.clip . #x018a) (vnd.font-fontforge-sfd . #x018b) (vnd.framemaker . #x018c)
      (vnd.frogans.fnc . #x018d) (vnd.frogans.ltf . #x018e) (vnd.fsc.weblaunch . #x018f)
      (vnd.fujitsu.oasys . #x0190) (vnd.fujitsu.oasys2 . #x0191) (vnd.fujitsu.oasys3 . #x0192)
      (vnd.fujitsu.oasysgp . #x0193) (vnd.fujitsu.oasysprs . #x0194) (vnd.fujixerox.ART4 . #x0195)
      (vnd.fujixerox.ART-EX . #x0196) (vnd.fujixerox.ddd . #x0197) (vnd.fujixerox.docuworks . #x0198)
      (vnd.fujixerox.docuworks.binder . #x0199) (vnd.fujixerox.HBPL . #x019a) (vnd.fut-misnet . #x019b)
      (vnd.fuzzysheet . #x019c) (vnd.genomatix.tuxedo . #x019d) (vnd.geocube+xml . #x019e)
      (vnd.geogebra.file . #x019f) (vnd.geogebra.tool . #x01a0) (vnd.geometry-explorer . #x01a1)
      (vnd.geonext . #x01a2) (vnd.geoplan . #x01a3) (vnd.geospace . #x01a4)
      (vnd.globalplatform.card-content-mgt . #x01a5) (vnd.globalplatform.card-content-mgt-response . #x01a6) (vnd.gmx . #x01a7)
      (vnd.google-earth.kml+xml . #x01a8) (vnd.google-earth.kmz . #x01a9) (vnd.grafeq . #x01aa)
      (vnd.gridmp . #x01ab) (vnd.groove-account . #x01ac) (vnd.groove-help . #x01ad)
      (vnd.groove-identity-message . #x01ae) (vnd.groove-injector . #x01af) (vnd.groove-tool-message . #x01b0)
      (vnd.groove-tool-template . #x01b1) (vnd.groove-vcard . #x01b2) (vnd.hal+json . #x01b3)
      (vnd.hal+xml . #x01b4) (vnd.HandHeld-Entertainment+xml . #x01b5) (vnd.hbci . #x01b6)
      (vnd.hcl-bireports . #x01b7) (vnd.hhe.lesson-player . #x01b8) (vnd.hp-HPGL . #x01b9)
      (vnd.hp-hpid . #x01ba) (vnd.hp-hps . #x01bb) (vnd.hp-jlyt . #x01bc)
      (vnd.hp-PCL . #x01bd) (vnd.hp-PCLXL . #x01be) (vnd.httphone . #x01bf)
      (vnd.hydrostatix.sof-data . #x01c0) (vnd.hzn-3d-crossword . #x01c1) (vnd.ibm.afplinedata . #x01c2)
      (vnd.ibm.electronic-media . #x01c3) (vnd.ibm.MiniPay . #x01c4) (vnd.ibm.modcap . #x01c5)
      (vnd.ibm.rights-management . #x01c6) (vnd.ibm.secure-container . #x01c7) (vnd.iccprofile . #x01c8)
      (vnd.ieee.1905 . #x01c9) (vnd.igloader . #x01ca) (vnd.immervision-ivp . #x01cb)
      (vnd.immervision-ivu . #x01cc) (vnd.informedcontrol.rms+xml . #x01cd) (vnd.infotech.project . #x01ce)
      (vnd.infotech.project+xml . #x01cf) (vnd.informix-visionary . #x01d0) (vnd.innopath.wamp.notification . #x01d1)
      (vnd.insors.igm . #x01d2) (vnd.intercon.formnet . #x01d3) (vnd.intergeo . #x01d4)
      (vnd.intertrust.digibox . #x01d5) (vnd.intertrust.nncp . #x01d6) (vnd.intu.qbo . #x01d7)
      (vnd.intu.qfx . #x01d8) (vnd.iptc.g2.conceptitem+xml . #x01d9) (vnd.iptc.g2.knowledgeitem+xml . #x01da)
      (vnd.iptc.g2.newsitem+xml . #x01db) (vnd.iptc.g2.newsmessage+xml . #x01dc) (vnd.iptc.g2.packageitem+xml . #x01dd)
      (vnd.iptc.g2.planningitem+xml . #x01de) (vnd.ipunplugged.rcprofile . #x01df) (vnd.irepository.package+xml . #x01e0)
      (vnd.is-xpr . #x01e1) (vnd.isac.fcs . #x01e2) (vnd.jam . #x01e3)
      (vnd.japannet-directory-service . #x01e4) (vnd.japannet-jpnstore-wakeup . #x01e5) (vnd.japannet-payment-wakeup . #x01e6)
      (vnd.japannet-registration . #x01e7) (vnd.japannet-registration-wakeup . #x01e8) (vnd.japannet-setstore-wakeup . #x01e9)
      (vnd.japannet-verification . #x01ea) (vnd.japannet-verification-wakeup . #x01eb) (vnd.jcp.javame.midlet-rms . #x01ec)
      (vnd.jisp . #x01ed) (vnd.joost.joda-archive . #x01ee) (vnd.jsk.isdn-ngn . #x01ef)
      (vnd.kahootz . #x01f0) (vnd.kde.karbon . #x01f1) (vnd.kde.kchart . #x01f2)
      (vnd.kde.kformula . #x01f3) (vnd.kde.kivio . #x01f4) (vnd.kde.kontour . #x01f5)
      (vnd.kde.kpresenter . #x01f6) (vnd.kde.kspread . #x01f7) (vnd.kde.kword . #x01f8)
      (vnd.kenameaapp . #x01f9) (vnd.kidspiration . #x01fa) (vnd.Kinar . #x01fb)
      (vnd.koan . #x01fc) (vnd.kodak-descriptor . #x01fd) (vnd.las.las+xml . #x01fe)
      (vnd.liberty-request+xml . #x01ff) (vnd.llamagraphics.life-balance.desktop . #x0200) (vnd.llamagraphics.life-balance.exchange+xml . #x0201)
      (vnd.lotus-1-2-3 . #x0202) (vnd.lotus-approach . #x0203) (vnd.lotus-freelance . #x0204)
      (vnd.lotus-notes . #x0205) (vnd.lotus-organizer . #x0206) (vnd.lotus-screencam . #x0207)
      (vnd.lotus-wordpro . #x0208) (vnd.macports.portpkg . #x0209) (vnd.marlin.drm.actiontoken+xml . #x020a)
      (vnd.marlin.drm.conftoken+xml . #x020b) (vnd.marlin.drm.license+xml . #x020c) (vnd.marlin.drm.mdcf . #x020d)
      (vnd.mcd . #x020e) (vnd.medcalcdata . #x020f) (vnd.mediastation.cdkey . #x0210)
      (vnd.meridian-slingshot . #x0211) (vnd.MFER . #x0212) (vnd.mfmp . #x0213)
      (vnd.micrografx.flo . #x0214) (vnd.micrografx.igx . #x0215) (vnd.mif . #x0216)
      (vnd.minisoft-hp3000-save . #x0217) (vnd.mitsubishi.misty-guard.trustweb . #x0218) (vnd.Mobius.DAF . #x0219)
      (vnd.Mobius.DIS . #x021a) (vnd.Mobius.MBK . #x021b) (vnd.Mobius.MQY . #x021c)
      (vnd.Mobius.MSL . #x021d) (vnd.Mobius.PLC . #x021e) (vnd.Mobius.TXF . #x021f)
      (vnd.mophun.application . #x0220) (vnd.mophun.certificate . #x0221) (vnd.motorola.flexsuite . #x0222)
      (vnd.motorola.flexsuite.adsi . #x0223) (vnd.motorola.flexsuite.fis . #x0224) (vnd.motorola.flexsuite.gotap . #x0225)
      (vnd.motorola.flexsuite.kmr . #x0226) (vnd.motorola.flexsuite.ttc . #x0227) (vnd.motorola.flexsuite.wem . #x0228)
      (vnd.motorola.iprm . #x0229) (vnd.mozilla.xul+xml . #x022a) (vnd.ms-artgalry . #x022b)
      (vnd.ms-asf . #x022c) (vnd.ms-cab-compressed . #x022d) (vnd.mseq . #x022e)
      (vnd.ms-excel . #x022f) (vnd.ms-excel.addin.macroEnabled.12 . #x0230) (vnd.ms-excel.sheet.binary.macroEnabled.12 . #x0231)
      (vnd.ms-excel.sheet.macroEnabled.12 . #x0232) (vnd.ms-excel.template.macroEnabled.12 . #x0233) (vnd.ms-fontobject . #x0234)
      (vnd.ms-htmlhelp . #x0235) (vnd.ms-ims . #x0236) (vnd.ms-lrm . #x0237)
      (vnd.ms-office.activeX+xml . #x0238) (vnd.ms-officetheme . #x0239) (vnd.ms-playready.initiator+xml . #x023a)
      (vnd.ms-powerpoint . #x023b) (vnd.ms-powerpoint.addin.macroEnabled.12 . #x023c) (vnd.ms-powerpoint.presentation.macroEnabled.12 . #x023d)
      (vnd.ms-powerpoint.slide.macroEnabled.12 . #x023e) (vnd.ms-powerpoint.slideshow.macroEnabled.12 . #x023f) (vnd.ms-powerpoint.template.macroEnabled.12 . #x0240)
      (vnd.ms-project . #x0241) (vnd.ms-tnef . #x0242) (vnd.ms-wmdrm.lic-chlg-req . #x0243)
      (vnd.ms-wmdrm.lic-resp . #x0244) (vnd.ms-wmdrm.meter-chlg-req . #x0245) (vnd.ms-wmdrm.meter-resp . #x0246)
      (vnd.ms-word.document.macroEnabled.12 . #x0247) (vnd.ms-word.template.macroEnabled.12 . #x0248) (vnd.ms-works . #x0249)
      (vnd.ms-wpl . #x024a) (vnd.ms-xpsdocument . #x024b) (vnd.msign . #x024c)
      (vnd.multiad.creator . #x024d) (vnd.multiad.creator.cif . #x024e) (vnd.musician . #x024f)
      (vnd.music-niff . #x0250) (vnd.muvee.style . #x0251) (vnd.mynfc . #x0252)
      (vnd.ncd.control . #x0253) (vnd.ncd.reference . #x0254) (vnd.nervana . #x0255)
      (vnd.netfpx . #x0256) (vnd.neurolanguage.nlu . #x0257) (vnd.nintendo.nitro.rom . #x0258)
      (vnd.nitf . #x0259) (vnd.noblenet-directory . #x025a) (vnd.noblenet-sealer . #x025b)
      (vnd.noblenet-web . #x025c) (vnd.nokia.catalogs . #x025d) (vnd.nokia.conml+wbxml . #x025e)
      (vnd.nokia.conml+xml . #x025f) (vnd.nokia.iptv.config+xml . #x0260) (vnd.nokia.iSDS-radio-presets . #x0261)
      (vnd.nokia.landmark+wbxml . #x0262) (vnd.nokia.landmark+xml . #x0263) (vnd.nokia.landmarkcollection+xml . #x0264)
      (vnd.nokia.ncd . #x0265) (vnd.nokia.n-gage.ac+xml . #x0266) (vnd.nokia.n-gage.data . #x0267)
      (vnd.nokia.n-gage.symbian.install . #x0268) (vnd.nokia.pcd+wbxml . #x0269) (vnd.nokia.pcd+xml . #x026a)
      (vnd.nokia.radio-preset . #x026b) (vnd.nokia.radio-presets . #x026c) (vnd.novadigm.EDM . #x026d)
      (vnd.novadigm.EDX . #x026e) (vnd.novadigm.EXT . #x026f) (vnd.ntt-local.content-share . #x0270)
      (vnd.ntt-local.file-transfer . #x0271) (vnd.ntt-local.sip-ta_remote . #x0272) (vnd.ntt-local.sip-ta_tcp_stream . #x0273)
      (vnd.oasis.opendocument.chart . #x0274) (vnd.oasis.opendocument.chart-template . #x0275) (vnd.oasis.opendocument.database . #x0276)
      (vnd.oasis.opendocument.formula . #x0277) (vnd.oasis.opendocument.formula-template . #x0278) (vnd.oasis.opendocument.graphics . #x0279)
      (vnd.oasis.opendocument.graphics-template . #x027a) (vnd.oasis.opendocument.image . #x027b) (vnd.oasis.opendocument.image-template . #x027c)
      (vnd.oasis.opendocument.presentation . #x027d) (vnd.oasis.opendocument.presentation-template . #x027e) (vnd.oasis.opendocument.spreadsheet . #x027f)
      (vnd.oasis.opendocument.spreadsheet-template . #x0280) (vnd.oasis.opendocument.text . #x0281) (vnd.oasis.opendocument.text-master . #x0282)
      (vnd.oasis.opendocument.text-template . #x0283) (vnd.oasis.opendocument.text-web . #x0284) (vnd.obn . #x0285)
      (vnd.oftn.l10n+json . #x0286) (vnd.oipf.contentaccessdownload+xml . #x0287) (vnd.oipf.contentaccessstreaming+xml . #x0288)
      (vnd.oipf.cspg-hexbinary . #x0289) (vnd.oipf.dae.svg+xml . #x028a) (vnd.oipf.dae.xhtml+xml . #x028b)
      (vnd.oipf.mippvcontrolmessage+xml . #x028c) (vnd.oipf.pae.gem . #x028d) (vnd.oipf.spdiscovery+xml . #x028e)
      (vnd.oipf.spdlist+xml . #x028f) (vnd.oipf.ueprofile+xml . #x0290) (vnd.oipf.userprofile+xml . #x0291)
      (vnd.olpc-sugar . #x0292) (vnd.oma.bcast.associated-procedure-parameter+xml . #x0293) (vnd.oma.bcast.drm-trigger+xml . #x0294)
      (vnd.oma.bcast.imd+xml . #x0295) (vnd.oma.bcast.ltkm . #x0296) (vnd.oma.bcast.notification+xml . #x0297)
      (vnd.oma.bcast.provisioningtrigger . #x0298) (vnd.oma.bcast.sgboot . #x0299) (vnd.oma.bcast.sgdd+xml . #x029a)
      (vnd.oma.bcast.sgdu . #x029b) (vnd.oma.bcast.simple-symbol-container . #x029c) (vnd.oma.bcast.smartcard-trigger+xml . #x029d)
      (vnd.oma.bcast.sprov+xml . #x029e) (vnd.oma.bcast.stkm . #x029f) (vnd.oma.cab-address-book+xml . #x02a0)
      (vnd.oma.cab-feature-handler+xml . #x02a1) (vnd.oma.cab-pcc+xml . #x02a2) (vnd.oma.cab-subs-invite+xml . #x02a3)
      (vnd.oma.cab-user-prefs+xml . #x02a4) (vnd.oma.dcd . #x02a5) (vnd.oma.dcdc . #x02a6)
      (vnd.oma.dd2+xml . #x02a7) (vnd.oma.drm.risd+xml . #x02a8) (vnd.oma.group-usage-list+xml . #x02a9)
      (vnd.oma.pal+xml . #x02aa) (vnd.oma.poc.detailed-progress-report+xml . #x02ab) (vnd.oma.poc.final-report+xml . #x02ac)
      (vnd.oma.poc.groups+xml . #x02ad) (vnd.oma.poc.invocation-descriptor+xml . #x02ae) (vnd.oma.poc.optimized-progress-report+xml . #x02af)
      (vnd.oma.push . #x02b0) (vnd.oma.scidm.messages+xml . #x02b1) (vnd.oma.xcap-directory+xml . #x02b2)
      (vnd.omads-email+xml . #x02b3) (vnd.omads-file+xml . #x02b4) (vnd.omads-folder+xml . #x02b5)
      (vnd.omaloc-supl-init . #x02b6) (vnd.oma-scws-config . #x02b7) (vnd.oma-scws-http-request . #x02b8)
      (vnd.oma-scws-http-response . #x02b9) (vnd.openofficeorg.extension . #x02ba) (vnd.openxmlformats-officedocument.custom-properties+xml . #x02bb)
      (vnd.openxmlformats-officedocument.customXmlProperties+xml . #x02bc) (vnd.openxmlformats-officedocument.drawing+xml . #x02bd) (vnd.openxmlformats-officedocument.drawingml.chart+xml . #x02be)
      (vnd.openxmlformats-officedocument.drawingml.chartshapes+xml . #x02bf) (vnd.openxmlformats-officedocument.drawingml.diagramColors+xml . #x02c0) (vnd.openxmlformats-officedocument.drawingml.diagramData+xml . #x02c1)
      (vnd.openxmlformats-officedocument.drawingml.diagramLayout+xml . #x02c2) (vnd.openxmlformats-officedocument.drawingml.diagramStyle+xml . #x02c3) (vnd.openxmlformats-officedocument.extended-properties+xml . #x02c4)
      (vnd.openxmlformats-officedocument.presentationml.commentAuthors+xml . #x02c5) (vnd.openxmlformats-officedocument.presentationml.comments+xml . #x02c6) (vnd.openxmlformats-officedocument.presentationml.handoutMaster+xml . #x02c7)
      (vnd.openxmlformats-officedocument.presentationml.notesMaster+xml . #x02c8) (vnd.openxmlformats-officedocument.presentationml.notesSlide+xml . #x02c9) (vnd.openxmlformats-officedocument.presentationml.presentation . #x02ca)
      (vnd.openxmlformats-officedocument.presentationml.presentation.main+xml . #x02cb) (vnd.openxmlformats-officedocument.presentationml.presProps+xml . #x02cc) (vnd.openxmlformats-officedocument.presentationml.slide . #x02cd)
      (vnd.openxmlformats-officedocument.presentationml.slide+xml . #x02ce) (vnd.openxmlformats-officedocument.presentationml.slideLayout+xml . #x02cf) (vnd.openxmlformats-officedocument.presentationml.slideMaster+xml . #x02d0)
      (vnd.openxmlformats-officedocument.presentationml.slideshow . #x02d1) (vnd.openxmlformats-officedocument.presentationml.slideshow.main+xml . #x02d2) (vnd.openxmlformats-officedocument.presentationml.slideUpdateInfo+xml . #x02d3)
      (vnd.openxmlformats-officedocument.presentationml.tableStyles+xml . #x02d4) (vnd.openxmlformats-officedocument.presentationml.tags+xml . #x02d5) (vnd.openxmlformats-officedocument.presentationml.template . #x02d6)
      (vnd.openxmlformats-officedocument.presentationml.template.main+xml . #x02d7) (vnd.openxmlformats-officedocument.presentationml.viewProps+xml . #x02d8) (vnd.openxmlformats-officedocument.spreadsheetml.calcChain+xml . #x02d9)
      (vnd.openxmlformats-officedocument.spreadsheetml.chartsheet+xml . #x02da) (vnd.openxmlformats-officedocument.spreadsheetml.comments+xml . #x02db) (vnd.openxmlformats-officedocument.spreadsheetml.connections+xml . #x02dc)
      (vnd.openxmlformats-officedocument.spreadsheetml.dialogsheet+xml . #x02dd) (vnd.openxmlformats-officedocument.spreadsheetml.externalLink+xml . #x02de) (vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheDefinition+xml . #x02df)
      (vnd.openxmlformats-officedocument.spreadsheetml.pivotCacheRecords+xml . #x02e0) (vnd.openxmlformats-officedocument.spreadsheetml.pivotTable+xml . #x02e1) (vnd.openxmlformats-officedocument.spreadsheetml.queryTable+xml . #x02e2)
      (vnd.openxmlformats-officedocument.spreadsheetml.revisionHeaders+xml . #x02e3) (vnd.openxmlformats-officedocument.spreadsheetml.revisionLog+xml . #x02e4) (vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml . #x02e5)
      (vnd.openxmlformats-officedocument.spreadsheetml.sheet . #x02e6) (vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml . #x02e7) (vnd.openxmlformats-officedocument.spreadsheetml.sheetMetadata+xml . #x02e8)
      (vnd.openxmlformats-officedocument.spreadsheetml.styles+xml . #x02e9) (vnd.openxmlformats-officedocument.spreadsheetml.table+xml . #x02ea) (vnd.openxmlformats-officedocument.spreadsheetml.tableSingleCells+xml . #x02eb)
      (vnd.openxmlformats-officedocument.spreadsheetml.template . #x02ec) (vnd.openxmlformats-officedocument.spreadsheetml.template.main+xml . #x02ed) (vnd.openxmlformats-officedocument.spreadsheetml.userNames+xml . #x02ee)
      (vnd.openxmlformats-officedocument.spreadsheetml.volatileDependencies+xml . #x02ef) (vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml . #x02f0) (vnd.openxmlformats-officedocument.theme+xml . #x02f1)
      (vnd.openxmlformats-officedocument.themeOverride+xml . #x02f2) (vnd.openxmlformats-officedocument.vmlDrawing . #x02f3) (vnd.openxmlformats-officedocument.wordprocessingml.comments+xml . #x02f4)
      (vnd.openxmlformats-officedocument.wordprocessingml.document . #x02f5) (vnd.openxmlformats-officedocument.wordprocessingml.document.glossary+xml . #x02f6) (vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml . #x02f7)
      (vnd.openxmlformats-officedocument.wordprocessingml.endnotes+xml . #x02f8) (vnd.openxmlformats-officedocument.wordprocessingml.fontTable+xml . #x02f9) (vnd.openxmlformats-officedocument.wordprocessingml.footer+xml . #x02fa)
      (vnd.openxmlformats-officedocument.wordprocessingml.footnotes+xml . #x02fb) (vnd.openxmlformats-officedocument.wordprocessingml.numbering+xml . #x02fc) (vnd.openxmlformats-officedocument.wordprocessingml.settings+xml . #x02fd)
      (vnd.openxmlformats-officedocument.wordprocessingml.styles+xml . #x02fe) (vnd.openxmlformats-officedocument.wordprocessingml.template . #x02ff) (vnd.openxmlformats-officedocument.wordprocessingml.template.main+xml . #x0300)
      (vnd.openxmlformats-officedocument.wordprocessingml.webSettings+xml . #x0301) (vnd.openxmlformats-package.core-properties+xml . #x0302) (vnd.openxmlformats-package.digital-signature-xmlsignature+xml . #x0303)
      (vnd.openxmlformats-package.relationships+xml . #x0304) (vnd.orange.indata . #x0305) (vnd.osa.netdeploy . #x0306)
      (vnd.osgeo.mapguide.package . #x0307) (vnd.osgi.bundle . #x0308) (vnd.osgi.dp . #x0309)
      (vnd.osgi.subsystem . #x030a) (vnd.otps.ct-kip+xml . #x030b) (vnd.palm . #x030c)
      (vnd.paos.xml . #x030d) (vnd.pawaafile . #x030e) (vnd.pg.format . #x030f)
      (vnd.pg.osasli . #x0310) (vnd.piaccess.application-licence . #x0311) (vnd.picsel . #x0312)
      (vnd.pmi.widget . #x0313) (vnd.poc.group-advertisement+xml . #x0314) (vnd.pocketlearn . #x0315)
      (vnd.powerbuilder6 . #x0316) (vnd.powerbuilder6-s . #x0317) (vnd.powerbuilder7 . #x0318)
      (vnd.powerbuilder75 . #x0319) (vnd.powerbuilder75-s . #x031a) (vnd.powerbuilder7-s . #x031b)
      (vnd.preminet . #x031c) (vnd.previewsystems.box . #x031d) (vnd.proteus.magazine . #x031e)
      (vnd.publishare-delta-tree . #x031f) (vnd.pvi.ptid1 . #x0320) (vnd.pwg-multiplexed . #x0321)
      (vnd.pwg-xhtml-print+xml . #x0322) (vnd.qualcomm.brew-app-res . #x0323) (vnd.Quark.QuarkXPress . #x0324)
      (vnd.quobject-quoxdocument . #x0325) (vnd.radisys.moml+xml . #x0326) (vnd.radisys.msml-audit-conf+xml . #x0327)
      (vnd.radisys.msml-audit-conn+xml . #x0328) (vnd.radisys.msml-audit-dialog+xml . #x0329) (vnd.radisys.msml-audit-stream+xml . #x032a)
      (vnd.radisys.msml-audit+xml . #x032b) (vnd.radisys.msml-conf+xml . #x032c) (vnd.radisys.msml-dialog-base+xml . #x032d)
      (vnd.radisys.msml-dialog-fax-detect+xml . #x032e) (vnd.radisys.msml-dialog-fax-sendrecv+xml . #x032f) (vnd.radisys.msml-dialog-group+xml . #x0330)
      (vnd.radisys.msml-dialog-speech+xml . #x0331) (vnd.radisys.msml-dialog-transform+xml . #x0332) (vnd.radisys.msml-dialog+xml . #x0333)
      (vnd.radisys.msml+xml . #x0334) (vnd.rainstor.data . #x0335) (vnd.rapid . #x0336)
      (vnd.realvnc.bed . #x0337) (vnd.recordare.musicxml . #x0338) (vnd.recordare.musicxml+xml . #x0339)
      (vnd.RenLearn.rlprint . #x033a) (vnd.rig.cryptonote . #x033b) (vnd.route66.link66+xml . #x033c)
      (vnd.rs-274x . #x033d) (vnd.ruckus.download . #x033e) (vnd.s3sms . #x033f)
      (vnd.sailingtracker.track . #x0340) (vnd.sbm.cid . #x0341) (vnd.sbm.mid2 . #x0342)
      (vnd.scribus . #x0343) (vnd.sealed.3df . #x0344) (vnd.sealed.csf . #x0345)
      (vnd.sealed.doc . #x0346) (vnd.sealed.eml . #x0347) (vnd.sealed.mht . #x0348)
      (vnd.sealed.net . #x0349) (vnd.sealed.ppt . #x034a) (vnd.sealed.tiff . #x034b)
      (vnd.sealed.xls . #x034c) (vnd.sealedmedia.softseal.html . #x034d) (vnd.sealedmedia.softseal.pdf . #x034e)
      (vnd.seemail . #x034f) (vnd.sema . #x0350) (vnd.semd . #x0351)
      (vnd.semf . #x0352) (vnd.shana.informed.formdata . #x0353) (vnd.shana.informed.formtemplate . #x0354)
      (vnd.shana.informed.interchange . #x0355) (vnd.shana.informed.package . #x0356) (vnd.SimTech-MindMapper . #x0357)
      (vnd.smaf . #x0358) (vnd.smart.notebook . #x0359) (vnd.smart.teacher . #x035a)
      (vnd.software602.filler.form+xml . #x035b) (vnd.software602.filler.form-xml-zip . #x035c) (vnd.solent.sdkm+xml . #x035d)
      (vnd.spotfire.dxp . #x035e) (vnd.spotfire.sfs . #x035f) (vnd.sss-cod . #x0360)
      (vnd.sss-dtf . #x0361) (vnd.sss-ntf . #x0362) (vnd.stepmania.package . #x0363)
      (vnd.stepmania.stepchart . #x0364) (vnd.street-stream . #x0365) (vnd.sun.wadl+xml . #x0366)
      (vnd.sus-calendar . #x0367) (vnd.svd . #x0368) (vnd.swiftview-ics . #x0369)
      (vnd.syncml.dm.notification . #x036a) (vnd.syncml.dmddf+xml . #x036b) (vnd.syncml.dmtnds+wbxml . #x036c)
      (vnd.syncml.dmtnds+xml . #x036d) (vnd.syncml.dmddf+wbxml . #x036e) (vnd.syncml.dm+wbxml . #x036f)
      (vnd.syncml.dm+xml . #x0370) (vnd.syncml.ds.notification . #x0371) (vnd.syncml+xml . #x0372)
      (vnd.tao.intent-module-archive . #x0373) (vnd.tcpdump.pcap . #x0374) (vnd.tmobile-livetv . #x0375)
      (vnd.trid.tpt . #x0376) (vnd.triscape.mxs . #x0377) (vnd.trueapp . #x0378)
      (vnd.truedoc . #x0379) (vnd.ubisoft.webplayer . #x037a) (vnd.ufdl . #x037b)
      (vnd.uiq.theme . #x037c) (vnd.umajin . #x037d) (vnd.unity . #x037e)
      (vnd.uoml+xml . #x037f) (vnd.uplanet.alert . #x0380) (vnd.uplanet.alert-wbxml . #x0381)
      (vnd.uplanet.bearer-choice . #x0382) (vnd.uplanet.bearer-choice-wbxml . #x0383) (vnd.uplanet.cacheop . #x0384)
      (vnd.uplanet.cacheop-wbxml . #x0385) (vnd.uplanet.channel . #x0386) (vnd.uplanet.channel-wbxml . #x0387)
      (vnd.uplanet.list . #x0388) (vnd.uplanet.listcmd . #x0389) (vnd.uplanet.listcmd-wbxml . #x038a)
      (vnd.uplanet.list-wbxml . #x038b) (vnd.uplanet.signal . #x038c) (vnd.vcx . #x038d)
      (vnd.vd-study . #x038e) (vnd.vectorworks . #x038f) (vnd.verimatrix.vcas . #x0390)
      (vnd.vidsoft.vidconference . #x0391) (vnd.visio . #x0392) (vnd.visionary . #x0393)
      (vnd.vividence.scriptfile . #x0394) (vnd.vsf . #x0395) (vnd.wap.sic . #x0396)
      (vnd.wap.slc . #x0397) (vnd.wap.wbxml . #x0398) (vnd.wap.wmlc . #x0399)
      (vnd.wap.wmlscriptc . #x039a) (vnd.webturbo . #x039b) (vnd.wfa.wsc . #x039c)
      (vnd.wmc . #x039d) (vnd.wmf.bootstrap . #x039e) (vnd.wolfram.mathematica . #x039f)
      (vnd.wolfram.mathematica.package . #x03a0) (vnd.wolfram.player . #x03a1) (vnd.wordperfect . #x03a2)
      (vnd.wqd . #x03a3) (vnd.wrq-hp3000-labelled . #x03a4) (vnd.wt.stf . #x03a5)
      (vnd.wv.csp+xml . #x03a6) (vnd.wv.csp+wbxml . #x03a7) (vnd.wv.ssp+xml . #x03a8)
      (vnd.xara . #x03a9) (vnd.xfdl . #x03aa) (vnd.xfdl.webform . #x03ab)
      (vnd.xmi+xml . #x03ac) (vnd.xmpie.cpkg . #x03ad) (vnd.xmpie.dpkg . #x03ae)
      (vnd.xmpie.plan . #x03af) (vnd.xmpie.ppkg . #x03b0) (vnd.xmpie.xlim . #x03b1)
      (vnd.yamaha.hv-dic . #x03b2) (vnd.yamaha.hv-script . #x03b3) (vnd.yamaha.hv-voice . #x03b4)
      (vnd.yamaha.openscoreformat.osfpvg+xml . #x03b5) (vnd.yamaha.openscoreformat . #x03b6) (vnd.yamaha.remote-setup . #x03b7)
      (vnd.yamaha.smaf-audio . #x03b8) (vnd.yamaha.smaf-phrase . #x03b9) (vnd.yamaha.through-ngn . #x03ba)
      (vnd.yamaha.tunnel-udpencap . #x03bb) (vnd.yellowriver-custom-menu . #x03bc) (vnd.zul . #x03bd)
      (vnd.zzazz.deck+xml . #x03be) (voicexml+xml . #x03bf) (vq-rtcpxr . #x03c0)
      (watcherinfo+xml . #x03c1) (whoispp-query . #x03c2) (whoispp-response . #x03c3)
      (widget . #x03c4) (wita . #x03c5) (wordperfect5.1 . #x03c6)
      (wsdl+xml . #x03c7) (wspolicy+xml . #x03c8) (x400-bp . #x03c9)
      (xcap-att+xml . #x03ca) (xcap-caps+xml . #x03cb) (xcap-diff+xml . #x03cc)
      (xcap-el+xml . #x03cd) (xcap-error+xml . #x03ce) (xcap-ns+xml . #x03cf)
      (xcon-conference-info-diff+xml . #x03d0) (xcon-conference-info+xml . #x03d1) (xenc+xml . #x03d2)
      (xhtml-voice+xml . #x03d3) (xhtml+xml . #x03d4) (xml . #x03d5)
      (xml-dtd . #x03d6) (xml-external-parsed-entity . #x03d7) (xmpp+xml . #x03d8)
      (xop+xml . #x03d9) (xslt+xml . #x03da) (xv+xml . #x03db)
      (yang . #x03dc) (yin+xml . #x03dd) (zip . #x03de)
      (zlib . #x03df))))

(define *imt-application-types-reverse*
  (make-parameter
    (map flip-pair (*imt-application-types*))))

(define *imt-audio-types*
  (make-parameter
    '((1d-interleaved-parityfec . #x0001) (32kadpcm . #x0002) (3gpp . #x0003)
      (3gpp2 . #x0004) (ac3 . #x0005) (AMR . #x0006)
      (AMR-WB . #x0007) (amr-wb+ . #x0008) (asc . #x0009)
      (ATRAC-ADVANCED-LOSSLESS . #x000a) (ATRAC-X . #x000b) (ATRAC3 . #x000c)
      (basic . #x000d) (BV16 . #x000e) (BV32 . #x000f)
      (clearmode . #x0010) (CN . #x0011) (DAT12 . #x0012)
      (dls . #x0013) (dsr-es201108 . #x0014) (dsr-es202050 . #x0015)
      (dsr-es202211 . #x0016) (dsr-es202212 . #x0017) (DV . #x0018)
      (DVI4 . #x0019) (eac3 . #x001a) (EVRC . #x001b)
      (EVRC0 . #x001c) (EVRC1 . #x001d) (EVRCB . #x001e)
      (EVRCB0 . #x001f) (EVRCB1 . #x0020) (EVRC-QCP . #x0021)
      (EVRCWB . #x0022) (EVRCWB0 . #x0023) (EVRCWB1 . #x0024)
      (fwdred . #x0025) (G719 . #x0026) (G722 . #x0027)
      (G7221 . #x0028) (G723 . #x0029) (G726-16 . #x002a)
      (G726-24 . #x002b) (G726-32 . #x002c) (G726-40 . #x002d)
      (G728 . #x002e) (G729 . #x002f) (G7291 . #x0030)
      (G729D . #x0031) (G729E . #x0032) (GSM . #x0033)
      (GSM-EFR . #x0034) (GSM-HR-08 . #x0035) (iLBC . #x0036)
      (ip-mr_v2.5 . #x0037) (L8 . #x0038) (L16 . #x0039)
      (L20 . #x003a) (L24 . #x003b) (LPC . #x003c)
      (mobile-xmf . #x003d) (MPA . #x003e) (mp4 . #x003f)
      (MP4A-LATM . #x0040) (mpa-robust . #x0041) (mpeg . #x0042)
      (mpeg4-generic . #x0043) (ogg . #x0044) (parityfec . #x0045)
      (PCMA . #x0046) (PCMA-WB . #x0047) (PCMU . #x0048)
      (PCMU-WB . #x0049) (prs.sid . #x004a) (QCELP . #x004b)
      (raptorfec . #x004c) (RED . #x004d) (rtp-enc-aescm128 . #x004e)
      (rtp-midi . #x004f) (rtx . #x0050) (SMV . #x0051)
      (SMV0 . #x0052) (SMV-QCP . #x0053) (sp-midi . #x0054)
      (speex . #x0055) (t140c . #x0056) (t38 . #x0057)
      (telephone-event . #x0058) (tone . #x0059) (UEMCLIP . #x005a)
      (ulpfec . #x005b) (VDVI . #x005c) (VMR-WB . #x005d)
      (vnd.3gpp.iufp . #x005e) (vnd.4SB . #x005f) (vnd.audiokoz . #x0060)
      (vnd.CELP . #x0061) (vnd.cisco.nse . #x0062) (vnd.cmles.radio-events . #x0063)
      (vnd.cns.anp1 . #x0064) (vnd.cns.inf1 . #x0065) (vnd.dece.audio . #x0066)
      (vnd.digital-winds . #x0067) (vnd.dlna.adts . #x0068) (vnd.dolby.heaac.1 . #x0069)
      (vnd.dolby.heaac.2 . #x006a) (vnd.dolby.mlp . #x006b) (vnd.dolby.mps . #x006c)
      (vnd.dolby.pl2 . #x006d) (vnd.dolby.pl2x . #x006e) (vnd.dolby.pl2z . #x006f)
      (vnd.dolby.pulse.1 . #x0070) (vnd.dra . #x0071) (vnd.dts . #x0072)
      (vnd.dts.hd . #x0073) (vnd.dvb.file . #x0074) (vnd.everad.plj . #x0075)
      (vnd.hns.audio . #x0076) (vnd.lucent.voice . #x0077) (vnd.ms-playready.media.pya . #x0078)
      (vnd.nokia.mobile-xmf . #x0079) (vnd.nortel.vbk . #x007a) (vnd.nuera.ecelp4800 . #x007b)
      (vnd.nuera.ecelp7470 . #x007c) (vnd.nuera.ecelp9600 . #x007d) (vnd.octel.sbc . #x007e)
      (vnd.qcelp . #x007f) (vnd.rhetorex.32kadpcm . #x0080) (vnd.rip . #x0081)
      (vnd.sealedmedia.softseal.mpeg . #x0082) (vnd.vmx.cvsd . #x0083) (vorbis . #x0084)
      (vorbis-config . #x0085))))

(define *imt-audio-types-reverse*
  (make-parameter
    (map flip-pair (*imt-audio-types*))))

(define *imt-image-types*
  (make-parameter
    '((cgm . #x0001) (fits . #x0002) (g3fax . #x0003)
      (gif . #x0004) (ief . #x0005) (jp2 . #x0006)
      (jpeg . #x0007) (jpm . #x0008) (jpx . #x0009)
      (ktx . #x000a) (naplps . #x000b) (png . #x000c)
      (prs.btif . #x000d) (prs.pti . #x000e) (pwg-raster . #x000f)
      (svg+xml . #x0010) (t38 . #x0011) (tiff . #x0012)
      (tiff-fx . #x0013) (vnd.adobe.photoshop . #x0014) (vnd.airzip.accelerator.azv . #x0015)
      (vnd.cns.inf2 . #x0016) (vnd.dece.graphic . #x0017) (vnd.djvu . #x0018)
      (vnd.dwg . #x0019) (vnd.dxf . #x001a) (vnd.dvb.subtitle . #x001b)
      (vnd.fastbidsheet . #x001c) (vnd.fpx . #x001d) (vnd.fst . #x001e)
      (vnd.fujixerox.edmics-mmr . #x001f) (vnd.fujixerox.edmics-rlc . #x0020) (vnd.globalgraphics.pgb . #x0021)
      (vnd.microsoft.icon . #x0022) (vnd.mix . #x0023) (vnd.ms-modi . #x0024)
      (vnd.net-fpx . #x0025) (vnd.radiance . #x0026) (vnd.sealed.png . #x0027)
      (vnd.sealedmedia.softseal.gif . #x0028) (vnd.sealedmedia.softseal.jpg . #x0029) (vnd.svf . #x002a)
      (vnd.wap.wbmp . #x002b) (vnd.xiff . #x002c))))

(define *imt-image-types-reverse*
  (make-parameter
    (map flip-pair (*imt-image-types*))))

(define *imt-message-types*
  (make-parameter
    '((CPIM . #x0001) (delivery-status . #x0002) (disposition-notification . #x0003)
      (external-body . #x0004) (feedback-report . #x0005) (global . #x0006)
      (global-delivery-status . #x0007) (global-disposition-notification . #x0008) (global-headers . #x0009)
      (http . #x000a) (imdn+xml . #x000b) (news . #x000c)
      (partial . #x000d) (rfc822 . #x000e) (s-http . #x000f)
      (sip . #x0010) (sipfrag . #x0011) (tracking-status . #x0012)
      (vnd.si.simp . #x0013))))

(define *imt-message-types-reverse*
  (make-parameter
    (map flip-pair (*imt-message-types*))))

(define *imt-model-types*
  (make-parameter
    '((iges . #x0001) (mesh . #x0002) (vnd.collada+xml . #x0003)
      (vnd.dwf . #x0004) (vnd.flatland.3dml . #x0005) (vnd.gdl . #x0006)
      (vnd.gs-gdl . #x0007) (vnd.gtw . #x0008) (vnd.moml+xml . #x0009)
      (vnd.mts . #x000a) (vnd.parasolid.transmit.binary . #x000b) (vnd.parasolid.transmit.text . #x000c)
      (vnd.vtu . #x000d) (vrml . #x000e))))

(define *imt-model-types-reverse*
  (make-parameter
    (map flip-pair (*imt-model-types*))))

(define *imt-multipart-types*
  (make-parameter
    '((alternative . #x0001) (appledouble . #x0002) (byteranges . #x0003)
      (digest . #x0004) (encrypted . #x0005) (form-data . #x0006)
      (header-set . #x0007) (mixed . #x0008) (parallel . #x0009)
      (related . #x000a) (report . #x000b) (signed . #x000c)
      (voice-message . #x000d))))

(define *imt-multipart-types-reverse*
  (make-parameter
    (map flip-pair (*imt-multipart-types*))))

(define *imt-text-types*
  (make-parameter
    '((1d-interleaved-parityfec . #x0001) (calendar . #x0002) (css . #x0003)
      (csv . #x0004) (directory . #x0005) (dns . #x0006)
      (ecmascript . #x0007) (enriched . #x0008) (fwdred . #x0009)
      (grammar-ref-list . #x000a) (html . #x000b) (javascript . #x000c)
      (jcr-cnd . #x000d) (mizar . #x000e) (n3 . #x000f)
      (parityfec . #x0010) (plain . #x0011) (prs.fallenstein.rst . #x0012)
      (prs.lines.tag . #x0013) (raptorfec . #x0014) (RED . #x0015)
      (rfc822-headers . #x0016) (richtext . #x0017) (rtf . #x0018)
      (rtp-enc-aescm128 . #x0019) (rtx . #x001a) (sgml . #x001b)
      (t140 . #x001c) (tab-separated-values . #x001d) (troff . #x001e)
      (turtle . #x001f) (ulpfec . #x0020) (uri-list . #x0021)
      (vcard . #x0022) (vnd.abc . #x0023) (vnd.curl . #x0024)
      (vnd.debian.copyright . #x0025) (vnd.DMClientScript . #x0026) (vnd.dvb.subtitle . #x0027)
      (vnd.esmertec.theme-descriptor . #x0028) (vnd.fly . #x0029) (vnd.fmi.flexstor . #x002a)
      (vnd.graphviz . #x002b) (vnd.in3d.3dml . #x002c) (vnd.in3d.spot . #x002d)
      (vnd.IPTC.NewsML . #x002e) (vnd.IPTC.NITF . #x002f) (vnd.latex-z . #x0030)
      (vnd.motorola.reflex . #x0031) (vnd.ms-mediapackage . #x0032) (vnd.net2phone.commcenter.command . #x0033)
      (vnd.radisys.msml-basic-layout . #x0034) (vnd.si.uricatalogue . #x0035) (vnd.sun.j2me.app-descriptor . #x0036)
      (vnd.trolltech.linguist . #x0037) (vnd.wap.si . #x0038) (vnd.wap.sl . #x0039)
      (vnd.wap.wml . #x003a) (vnd.wap.wmlscript . #x003b) (xml . #x003c)
      (xml-external-parsed-entity . #x003d))))

(define *imt-text-types-reverse*
  (make-parameter
    (map flip-pair (*imt-text-types*))))

(define *imt-video-types*
  (make-parameter
    '((1d-interleaved-parityfec . #x0001) (3gpp . #x0002) (3gpp2 . #x0003)
      (3gpp-tt . #x0004) (BMPEG . #x0005) (BT656 . #x0006)
      (CelB . #x0007) (DV . #x0008) (H261 . #x0009)
      (H263 . #x000a) (H263-1998 . #x000b) (H263-2000 . #x000c)
      (H264 . #x000d) (H264-RCDO . #x000e) (H264-SVC . #x000f)
      (JPEG . #x0010) (jpeg2000 . #x0011) (MJ2 . #x0012)
      (MP1S . #x0013) (MP2P . #x0014) (MP2T . #x0015)
      (mp4 . #x0016) (MP4V-ES . #x0017) (MPV . #x0018)
      (mpeg . #x0019) (mpeg4-generic . #x001a) (nv . #x001b)
      (ogg . #x001c) (parityfec . #x001d) (pointer . #x001e)
      (quicktime . #x001f) (raptorfec . #x0020) (raw . #x0021)
      (rtp-enc-aescm128 . #x0022) (rtx . #x0023) (SMPTE292M . #x0024)
      (ulpfec . #x0025) (vc1 . #x0026) (vnd.CCTV . #x0027)
      (vnd.dece.hd . #x0028) (vnd.dece.mobile . #x0029) (vnd.dece.mp4 . #x002a)
      (vnd.dece.pd . #x002b) (vnd.dece.sd . #x002c) (vnd.dece.video . #x002d)
      (vnd.directv.mpeg . #x002e) (vnd.directv.mpeg-tts . #x002f) (vnd.dlna.mpeg-tts . #x0030)
      (vnd.dvb.file . #x0031) (vnd.fvt . #x0032) (vnd.hns.video . #x0033)
      (vnd.iptvforum.1dparityfec-1010 . #x0034) (vnd.iptvforum.1dparityfec-2005 . #x0035) (vnd.iptvforum.2dparityfec-1010 . #x0036)
      (vnd.iptvforum.2dparityfec-2005 . #x0037) (vnd.iptvforum.ttsavc . #x0038) (vnd.iptvforum.ttsmpeg2 . #x0039)
      (vnd.motorola.video . #x003a) (vnd.motorola.videop . #x003b) (vnd.mpegurl . #x003c)
      (vnd.ms-playready.media.pyv . #x003d) (vnd.nokia.interleaved-multimedia . #x003e) (vnd.nokia.videovoip . #x003f)
      (vnd.objectvideo . #x0040) (vnd.sealed.mpeg1 . #x0041) (vnd.sealed.mpeg4 . #x0042)
      (vnd.sealed.swf . #x0043) (vnd.sealedmedia.softseal.mov . #x0044) (vnd.uvvu.mp4 . #x0045)
      (vnd.vivo . #x0046))))

(define *imt-video-types-reverse*
  (make-parameter
    (map flip-pair (*imt-video-types*))))

(define (imt-encode typestr)
  (let* ((parts (string-split typestr "/"))
         (type-sym (string->symbol (car parts)))
         (subtype-sym (string->symbol (cadr parts)))
         (subtype-table
           (case type-sym
             ((application) (*imt-application-types*))
             ((audio) (*imt-audio-types*))
             ((image) (*imt-image-types*))
             ((message) (*imt-message-types*))
             ((model) (*imt-model-types*))
             ((multipart) (*imt-multipart-types*))
             ((text) (*imt-text-types*))
             ((video) (*imt-video-types*))
             (else (eprintf "Media type '~A' is not registered.")))))
    (+ (alist-ref type-sym (*imt-types*))
       (alist-ref subtype-sym subtype-table))))

(define (imt-type-sym idx)
  (let ((idx* (bitwise-and idx #xf000)))
    (alist-ref idx* (*imt-types-reverse*))))

(define (imt-subtype-sym idx)
  (let ((idx* (bitwise-and idx #x0fff))
        (subtype-table
          (case (imt-main-type idx)
            ((application) (*imt-application-types-reverse*))
            ((audio) (*imt-audio-types-reverse*))
            ((image) (*imt-image-types-reverse*))
            ((message) (*imt-message-types-reverse*))
            ((model) (*imt-model-types-reverse*))
            ((multipart) (*imt-multipart-types-reverse*))
            ((text) (*imt-text-types-reverse*))
            ((video) (*imt-video-types-reverse*)))))
    (alist-ref idx* subtype-table)))

(define (imt-decode idx)
  (let ((main (imt-type-sym idx))
        (sub (imt-subtype-sym idx)))
    (string-append
      (symbol->string main) "/" (symbol->string sub))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; --  AUTOMATIC PATHS  ---------------------------------------------------

(define (auto-path resource-id #!optional (root (*file-storage-path*)))
  (let* ((byte1
           (arithmetic-shift resource-id -24))
         (byte2
           (arithmetic-shift (bitwise-and resource-id #xff0000) -16))
         (byte3
           (arithmetic-shift (bitwise-and resource-id #xff00) -8))
         (byte4
           (bitwise-and resource-id #xff))
         (bstr1
           (number->string byte1 16))
         (bstr2
           (number->string byte2 16))
         (bstr3
           (number->string byte3 16))
         (bstr4
           (number->string byte4 16))
         (path1 (zpad bstr1 2))
         (path2 (zpad bstr2 2))
         (path3 (zpad bstr3 2))
         (path4 (zpad bstr4 2)))
    (foldl make-pathname root (list path1 path2 path3 path4))))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO



;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

(define-record-type
  fileref (make-fileref imt-code path) fileref? 
  (imt-code fileref-imt-code) (path fileref-path))

(register-prop-type
  'fileref
  to-string: (lambda (fr)
               (let* ((code (fileref-imt-code fr))
                      (byte1 (arithmetic-shift code -8))
                      (byte2 (bitwise-and code #xff))
                      (path (fileref-path fr)))
                 (with-output-to-string
                   (lambda ()
                     (write-byte byte1)
                     (write-byte byte2)
                     (when path (write-string path))))))
  from-string: (lambda (s)
                 (with-input-from-string s
                   (lambda ()
                     (let* ((byte1 (read-byte))
                            (byte2 (read-byte))
                            (code (+ (arithmetic-shift byte1 8) byte2))
                            (path (read-string))
                            (path* (and (> (string-length path) 0)
                                        path)))
                       (make-fileref code path*)))))
  validator: fileref?)

(define (get-fileref-imt fr)
  (imt-decode fileref-imt-code fr))

(define (get-fileref-path fr res-id prop-name)
  (or (fileref-path fr)
      (make-pathname (auto-path res-id) prop-name)))

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

) ; END MODULE

;;; IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
;;; ------------------------------------------------------------------------

;;; OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO

;;; ========================================================================
;;; ------------------------------------------------------------------------


