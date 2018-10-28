;  (setq vm-spool-files '("/var/mail/cts" 
;                         ;; "mail.internetcds.com:110:pass:cts:*"
;                         "mail.cdsnet.net:110:pass:cts:*"))
(setq vm-spool-files 
      '(("~/INBOX"               "~/ProcMailFolders/INBOX"   "~/INBOX.CRASH")
        ("~/MailFolders/INBOX.GENERAL"               
         "~/ProcMailFolders/INBOX.GENERAL"   
         "~/MailFolders/INBOX.GENERAL.CRASH")
        ("~/MailFolders/active_customer_summary"  "~/ProcMailFolders/active_customer_summary"   "~/ProcMailFolders/active_customer_summary.CRASH")
                                        ;          ("~/INBOX"               "mail.cdsnet.net:110:pass:cts:*" "~/POPBOX.CRASH")
        ;;          ("~/MailFolders/junk"    "~/ProcMailFolders/junk"    "~/ProcMailFolders/junk.CRASH")
        ("~/MailFolders/gnatchat" "~/ProcMailFolders/gnatchat" "~/ProcMailFolders/gnatchat.CRASH")
        ("~/MailFolders/junk" "~/ProcMailFolders/junk" "~/ProcMailFolders/junk.CRASH")
        ("~/MailFolders/spam_bounces" "~/ProcMailFolders/spam_bounces" "~/ProcMailFolders/spam_bounces.CRASH")
        ("~/MailFolders/visual_builder" "~/ProcMailFolders/visual_builder" "~/ProcMailFolders/visual_builder.CRASH")
        ("~/MailFolders/visual_builder" "~/ProcMailFolders/visual_buillder" "~/ProcMailFolders/visual_buillder.CRASH")
        ("~/MailFolders/msqlperl" "~/ProcMailFolders/msqlperl" "~/ProcMailFolders/msqlperl.CRASH")
        ("~/MailFolders/freebsd-hackers" "~/ProcMailFolders/freebsd-hackers" "~/ProcMailFolders/freebsd-hackers.CRASH")
        ("~/MailFolders/freebsd-security" "~/ProcMailFolders/freebsd-security" "~/ProcMailFolders/freebsd-security.CRASH")
        ("~/MailFolders/freebsd-small" "~/ProcMailFolders/freebsd-small" "~/ProcMailFolders/freebsd-small.CRASH")
        ("~/MailFolders/freebsd-smp" "~/ProcMailFolders/freebsd-smp" "~/ProcMailFolders/freebsd-smp.CRASH")
        ("~/MailFolders/pts-users" "~/ProcMailFolders/pts-users" "~/ProcMailFolders/pts-users.CRASH")
        ("~/MailFolders/jdctechtips" "~/ProcMailFolders/jdctechtips" "~/ProcMailFolders/jdctechtips.CRASH")
        ("~/MailFolders/adduser"  "~/ProcMailFolders/adduser"  "~/ProcMailFolders/adduser.CRASH")))