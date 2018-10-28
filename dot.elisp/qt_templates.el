;;;; qt_templates.el
;;;; Copyright 2008 Craig Spannring
;;;;
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; 3. The name of Craig Spannring may not be used to endorse or promote
;;;;    products derived from this software without specific prior
;;;;    written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY CRAIG SPANNRING ``AS IS'' AND
;;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL CRAIG SPANNRING BE LIABLE
;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;;; SUCH DAMAGE.
;;;;
;;;;
;;;; Collections of functions that generate files with Qt templates. 
;;;; 
;;;; Functions:
;;;;  qt-create-designer-plugin
;;;;


(defconst _qt-pro-template 
"# -*- mode: makefile -*- 
# Initially created by qt-create-designer-plugin %(date)s
TEMPLATE     = lib
CONFIG      += designer plugin release
TARGET       = %(widget-name)sPlugin
MAKEFILE     = %(widget-name)sPlugin.mk

# uncomment if you  need this directory in the -I compiler options
# INCLUDEPATH += %(widget-h-dir-relative-path)s

# uncomment if you need this path for the moc compiler
# DEPENDPATH  += %(widget-h-dir-relative-path)s


# Input
HEADERS     += %(widget-name)sPlugin.h
HEADERS     += %(widget-h-dir-relative-path)s%(widget-h-basename)s


SOURCES     += %(widget-name)sPlugin.cpp
SOURCES     += %(widget-cpp-dir-relative-path)s%(widget-cpp-basename)s

# uncomment if the plug in needs a resource file (e.g. an icon)
# RESOURCES += %(widget-name)sPluginResources.qrc

DESTDIR      = ./plugin/designer

" "Template used to create the qmake project file for the plug-in"
)


(defconst _qt-cpp-template 
"/**
 * Designer plug-in for %(widget-name)s
 *
 * Initially created by qt-create-designer-plugin %(date)s
 */ 

#include <QtPlugin>
#include \"%(widget-name)sPlugin.h\"
#include \"%(widget-h-dir-relative-path)s%(widget-h-basename)s\"

/** 
 * Constructor for the plug in
 */ 
%(widget-name)sPlugin::%(widget-name)sPlugin(QObject *parent)
: QObject(parent)
{
}

/** 
 * Name of this widget that is used in the designer's widget list
 */
QString %(widget-name)sPlugin::name() const 
{
  return \"%(widget-name)s\";
}

/**
 * Name of the include file that defines the widget
 */
QString %(widget-name)sPlugin::includeFile() const
{
  return \"%(widget-h-basename)s\";
}

/**
 * Name of the group in the designer's widget list where this widget will be placed
 */
QString %(widget-name)sPlugin::group() const
{
  return  \"Custom Widgets\"; /* TODO- Set the group name */
}


/**
 * Icon used for this widget in the designer's widget list
 */
QIcon   %(widget-name)sPlugin::icon() const
{
  return QIcon(/* TODO- Add an icon to use for this widget in the designer */);
}


/** 
 * Tool tip displayed then mouse hovers over the widget icon in the designer's widget list
 * @note This is usually one line.
 */
QString %(widget-name)sPlugin::toolTip() const
{
  return \"\"; /* TODO- Write a brief description for this widget. */ 
}


/** 
 * Description of the widget 
 */
QString %(widget-name)sPlugin::whatsThis() const
{
  return \"\"; /* TODO- Describe the widget */
}


/**
 * Is the widget meant to be used as a container?
 */
bool    %(widget-name)sPlugin::isContainer() const
{
  return false; 
}


/** 
 * Create the widget
 *
 * This is called by the designer whenever it needs to display 
 * the widget.
 */
QWidget *%(widget-name)sPlugin::createWidget(QWidget *parent)
{
  return new %(widget-name)s(parent);
}


Q_EXPORT_PLUGIN2(%(widget-name)sPlugin, %(widget-name)sPlugin)

" "Template used to create the source file for the plug-in"
)


(defconst _qt-h-template 
"/**
 * Designer plug-in for %(widget-name)s
 *
 * Initially created by qt-create-designer-plugin %(date)s
 */ 
#ifndef %(include_guard)s
#define %(include_guard)s


#include <QDesignerCustomWidgetInterface>

class QDESIGNER_WIDGET_EXPORT %(widget-name)sPlugin 
: public QObject, public QDesignerCustomWidgetInterface
{
    Q_OBJECT
    Q_INTERFACES(QDesignerCustomWidgetInterface);

  public:
    %(widget-name)sPlugin(QObject *parent = 0);
    
    QString name() const; 
    QString includeFile() const; 
    QString group() const; 
    QIcon   icon() const; 
    QString toolTip() const; 
    QString whatsThis() const; 
    bool    isContainer() const; 
    QWidget *createWidget(QWidget *parent); 
};


#endif

" "Template used to create the header file for the plug-in"
)

(defun _qt-python-like-format (fmt rep)
  "Format a string using a python-like string replacement
   
   FMT is a python-like format string.  
   REP is a list of dotted pairs.  The first item in the pair is a 
       string that is the name searched for in FMT.  The second 
       item is the value that should be substituted into the format string.

   Example:
      (_qt-python-like-format \"The %(item)s is %(color)s\" 
                              '(list (cons \"item\"  \"car\")
                                     (cons \"color\"  \"red\")))
      returns the string \"The car is red\"

   Note- Currently the only support format character is 's', the string character.
  "
  (save-excursion  
    (let  (result (tmp-buffer (generate-new-buffer "*FMT-TEMP-BUFF*")))
      (set-buffer tmp-buffer)
      (insert fmt)
      (while rep
        (let ((item (car rep)))
          (let 
              ((FROM-STRING (format "%%(%s)s" (car item)))
               (TO-STRING   (cdr item)))
            
            (goto-char (point-min))
            (while (search-forward FROM-STRING nil t)
              (replace-match TO-STRING nil t))))
        (setq rep (cdr rep)))
      (setq result (buffer-substring (point-min) (point-max)))
      (kill-buffer tmp-buffer)
      result)))


(defun _qt-read-args-for-create-designer-plugin ()
  "Read the arguments required for qt-create-designer-plugin from the minibuf"

  (let (cpp-file h-file widget-name location)
    (setq cpp-file (read-file-name "Widget Source File: " nil nil t))
    (setq h-file   (read-file-name "Widget Header File: " 
                                   (file-name-directory cpp-file)
                                   nil 
                                   t
                                   (concat (file-name-sans-extension (file-name-nondirectory cpp-file)) ".h")))
    (setq widget-name (read-string "Widget Name: " (file-name-nondirectory (file-name-sans-extension cpp-file))))
    (setq location (read-directory-name "Plug-in Location: " nil nil t))
    (list cpp-file h-file widget-name location)))



(defun qt-create-designer-plugin (cpp-file
                                  h-file
                                  widget-name 
                                  location)
  "Create the necessary files for a Qt designer plug-in for a custom widget
    
   CPP-FILE is the name of the widget's implementation file.
   H-FILE is the name of the widget's header file.
   WIDGET-NAME is the name of the custom widget
   LOCATION is where the plug-in files will be created. 
  "

  
  (interactive (_qt-read-args-for-create-designer-plugin))

  ;;
  ;; Convert the location string into a directory object
  ;; 
  (setq location (file-name-as-directory location))
  

  (let ((plugin_pro (concat location (concat widget-name "Plugin.pro")))  ; name of the qmake project file
        (plugin_h   (concat location (concat widget-name "Plugin.h")))    ; name of the plug-in header file
        (plugin_cpp (concat location (concat widget-name "Plugin.cpp")))) ; name of the plug-in source file

    ;;
    ;; Check the parameters.  The directory must already exist, the
    ;; files must not.
    ;;
    (cond 
     ((not (file-directory-p location)) (error "Location does not exist"))
     ((file-exists-p plugin_pro)        (error "Can not overwrite existing .pro file"))
     ((file-exists-p plugin_cpp)        (error "Can not overwrite existing .cpp file"))
     ((file-exists-p plugin_h)          (error "Can not overwrite existing .h file")))
    
    (let ((buffer_h     (find-file-noselect plugin_h))   ; create a buffer for the header file 
          (buffer_cpp   (find-file-noselect plugin_cpp)) ; create buffer for the source file 
          (buffer_pro   (find-file-noselect plugin_pro)) ; create buffer for the project file  
          
          ;; 
          ;; List of (name . value) pairs used for string replacement in the templates
          ;; 
          (replacements (list (cons "widget-name"               widget-name)
                              (cons "widget-cpp-dir-relative-path" 
                                    (file-relative-name (file-name-directory cpp-file)
                                                        location))
                              (cons "widget-cpp-basename" (file-name-nondirectory cpp-file))
                              (cons "widget-h-dir-relative-path"   
                                    (file-relative-name (file-name-directory h-file)
                                                        location))
                              (cons "widget-h-basename" (file-name-nondirectory h-file))
                              (cons "include_guard"    (concat "HEADER_"
                                                               (upcase (file-name-sans-extension
                                                                        (file-name-nondirectory h-file)))
                                                               "_h"))
                              (cons "date"                         (current-time-string)))))
      
      ;;
      ;; Add stuff to the .h file
      ;; 
      (set-buffer buffer_h)
      (insert (_qt-python-like-format _qt-h-template replacements))
      
      ;;
      ;; Add stuff to the .cpp file
      ;; 
      (set-buffer buffer_cpp)
      (insert (_qt-python-like-format _qt-cpp-template replacements))
      
      ;;
      ;; Add stuff to the .pro file
      ;; 
      (set-buffer buffer_pro)
      (insert (_qt-python-like-format _qt-pro-template replacements))
      
      
      ;;
      ;; Not sure if we really want to switch to the buffers or not
      ;; Should think about the next set of lines a bit more. 
      ;; 
      (switch-to-buffer buffer_h)
      (switch-to-buffer buffer_cpp)
      (switch-to-buffer buffer_pro))))


