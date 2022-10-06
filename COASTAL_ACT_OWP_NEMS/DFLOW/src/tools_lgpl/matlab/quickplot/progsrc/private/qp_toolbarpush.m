function H=qp_toolbarpush(hP,cmd,sep,tooltip,prog)
%QP_TOOLBARPUSH Create a toolbar push button.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2022 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_toolbarpush.m $
%   $Id: qp_toolbarpush.m 140618 2022-01-12 13:12:04Z klapwijk $

offon={'off','on'};
sep=offon{1+sep};
if nargin<5
    prog = 'd3d_qp';
end
H = uipushtool('Parent',hP, ...
    'CData',qp_icon(cmd,'nan'), ...
    'ClickedCallback',sprintf('%s %s',prog,cmd), ...
    'Separator',sep, ...
    'Serializable','off', ...
    'Tag',cmd);
qp_tooltip(H,tooltip)