function varargout=trirst(cmd,varargin)
%TRIRST Read/write Delft3D-FLOW restart file.
%   [H,U,V,...]=TRIRST('read',FILENAME,SIZE)
%   or
%   [H,U,V,...]=TRIRST('read',FILENAME,GRID)
%   where GRID was generated by WLGRID. For a restart file of 3D
%   simulations you need to specify the number of layers per field as an
%   array NLAYERS:
%   [H,U,V,...]=TRIRST('read',FILENAME,GRID,NLAYERS)
%   where NLAYERS=[NLAYERH , NLAYERU , NLAYERV , ...]
%
%   TRIRST('write',FILENAME,H,U,V,...)
%   TRIRST('write',FILENAME,PLATFORM,PREC,H,U,V,...)
%   where PLATFORM can be 'pc' or 'unix', and PREC can be 32 or 64.
%   PLATFORM is by default equal to the computer on which MATLAB is
%   running. The PREC value indicates the precision of the values stored:
%   float32 or float64. Delft3D-FLOW typically uses float32 values for
%   restart files.

%   X=TRIRST('read',FILENAME,GRID,'all')
%   reads all datasets in the file assuming NLAYERS=1
%   and returns them in a structure with one field: Data.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/releases/140712/src/tools_lgpl/matlab/quickplot/progsrc/trirst.m $
%   $Id: trirst.m 140618 2022-01-12 13:12:04Z klapwijk $

if nargin==0
    if nargout>0
        varargout=cell(1,nargout);
    end
    return
end

switch lower(cmd)
    case 'read'
        if nargout>0
            varargout=cell(1,nargout);
            [varargout{:}]=Local_rstread(varargin{:});
        else
            Local_rstread(varargin{:});
        end
    case 'write'
        OK=Local_rstwrite(varargin{:});
        if nargout>0
            varargout={OK};
        end
    otherwise
        error('Unknown command')
end



function varargout=Local_rstread(filename,dimvar,threeD)
% RSTREAD reads data from restart file.
%
%    [H,U,V,...]=RSTREAD(FILENAME,DIMVAR,NLAYERS)
%

if nargin<2
    warning('No size or grid specified.')
    return
end

NRequested=nargout;
varargout=cell(1,NRequested);
if isstruct(dimvar) % new grid format G.X, G.Y, G.Enclosure
    dim=size(dimvar.X)+1;
elseif iscell(dimvar) % old grid format {X Y Enclosure}
    dim=size(dimvar{1})+1;
else
    dim=dimvar;
end

fid=fopen(filename,'r','b'); % Try UNIX format ...
T=fread(fid,1,'int32');
if (4*prod(dim))~=T && (8*prod(dim))~=T % If not a match ...
    fclose(fid);
    fid=fopen(filename,'r','l'); % Try PC format ...
    Tb=T;
    T=fread(fid,1,'int32');
end
if (4*prod(dim))==T
    prec = 'float32';
elseif (8*prod(dim))==T
    prec = 'float64';
else % If not a match ...
    fclose(fid);
    error('Specified size %ix%i=%i does not match\nrecord length %i or %i in file.',dim,prod(dim),T/4,Tb/4)
end
TRef = T;

if nargin==2
    threeD=1;
elseif isequal(lower(threeD),'all') && NRequested==1
    threeD=1;
    NRequested=inf;
else
    threeD=threeD(:);
    if length(threeD)<NRequested
        ui_message('warning',{'Length of NLAYERS array is less than expected number of fields.','Using NLAYERS=1 for remaining fields.'});
    end
    threeD(end+1)=1;
end

i=0;
ReadMore=2;
Y=[];
while ReadMore>1
    i=i+1;
    nlayer=threeD(min(i,length(threeD)));
    if isfinite(NRequested)
        Y=[];
    end
    for l=1:nlayer
        %
        % Assert that the block size is correct.
        %
        if T~=TRef
            fclose(fid);
            error('Size (%i) of record %i of quantity %i does not match expected size %i',T,l,i,TRef)
        end
        %
        % Record size is correct, so read the data
        %
        X=fread(fid,dim,prec);
        if ~isequal(size(X),dim)
            fclose(fid);
            Msg=sprintf('Out of data while reading layer %i of record %i.',l,i);
            if (i>NRequested)
                ui_message('warning',Msg);
                return
            else
                error(Msg)
            end
        end
        if isfinite(NRequested)
            Y(1:dim(1),1:dim(2),l)=X;
        else
            Y(i).Data=X;
        end
        [TMore,ReadMore]=fread(fid,2,'int32');
        % TMore(1) should equal the previous block size and hence it should
        % be equal to T. However, we need to verify whether the next block
        % size TMore(2) is correct (if it exists). The actual check happens
        % just before reading the block to have the correct context for the
        % error message; here we just save the second value (if it exists).
        T = TMore(2:end);
    end
    if isfinite(NRequested)
        varargout{i}=Y;
    end
end
if ~isfinite(NRequested)
    varargout{1}=Y;
end
if (i>NRequested) && (NRequested~=0)
    ui_message('warning','%i more data field(s) in the file.\n',i-NRequested)
elseif (i<NRequested) && isfinite(NRequested)
    error('Out of data while reading layer %i of record %i.\n',l,i)
end
fclose(fid);


function OK=Local_rstwrite(filename,varargin)
% RSTWRITE writes data to a restart file.
%
%    RSTWRITE(FILENAME,H,U,V,...)
%    RSTWRITE(FILENAME,'PC',H,U,V,...)
%    RSTWRITE(FILENAME,'UNIX',H,U,V,...)
%

if nargin<4 && ~isstruct(varargin{1})
    error('Not enough input arguments.')
end
PC = computer;
prec = 'float32';
Data = varargin;
for i = length(Data):-1:1
    if ischar(Data{i})
        str = Data{i};
        switch lower(str)
            case {'32bit','32 bit','float32','32'}
                prec = 'float32';
            case {'64bit','64 bit','float64','64'}
                prec = 'float64';
            otherwise
                PC = str;
        end
        Data(i) = [];
    elseif isnumeric(Data{i}) && numel(Data{i})==1
        if Data{i}==32
            prec = 'float32';
            Data(i) = [];
        elseif Data{i}==64
            prec = 'float64';
            Data(i) = [];
        end
    elseif isstruct(Data{i})
        % allow for a structure to be written, structure is identical to the one retrieved when reading with 'all'
        str = Data{i};
        nfields = length(str);
        % normally this should only occur if the structure is the only data
        % record, but let's be generic and just insert the records at the
        % current offset.
        nflds_after = length(Data)-i;
        % shift the already processed records
        Data(i+(nfields-1)+1:i+(nfields-1)+nflds_after) = Data(i+1:i+nflds_after);
        for j = nfields:-1:1
            Data{i+j-1} = str(j).Data;
        end
    end
end
% check consistency of dimensions
sz1 = cellfun('size',Data,1);
sz2 = cellfun('size',Data,2);
if ~all(sz1==sz1(1)) || ~all(sz2==sz2(1))
    error('Data sets should be of equal size')
end
% check computer and set number of bytes depending on precision
switch lower(PC)
    case {'pc','pcwin','dos','windows','l','ieee-le','pcwin64'}
        fid=fopen(filename,'w','l');
    case {'hp','sg','sgi','sgi64','unix','b','sol','sol2','ieee-be'}
        fid=fopen(filename,'w','b');
    otherwise
        error('Unsupported file format: %s.',PC)
end
if strcmp(prec,'float32')
    nbytes = 4;
else
    nbytes = 8;
end
% write actual data
T=nbytes*numel(Data{1});
for i=1:length(Data)
    szData=size(Data{i});
    nlayer=prod(szData(3:end));
    for l=1:nlayer
        fwrite(fid,T,'int32');
        fwrite(fid,Data{i}(:,:,l),prec);
        fwrite(fid,T,'int32');
    end
end
fclose(fid);
OK=1;
