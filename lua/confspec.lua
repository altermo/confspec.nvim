--- You can find the upstream project here: https://github.com/altermo/confspec.nvim

local M={}

---@alias confspec.spec
---|confspec.meta_spec.default.string_handler
---|table<any,confspec.spec>
---|confspec.fn

---@class confspec.opts
---@field err_severity number
---@field err_format confspec.err_format
---@field plugin_name string

---@alias confspec.err_format
---| 'table'
---| 'raw'
---| 'borderless'
---| 'default'
---| 'minimal'

---@class confspec.error
---@field severity number
---@field msg string

---@class (partial) confspec.data
---@field meta_spec confspec.meta_spec
---@field opts confspec.opts
---@field traceback string
---@field errors confspec.error[]

---@alias confspec.handler_fn (fun(data: confspec.data, conf: any?, spec: confspec.spec): boolean)|confspec.fn
---@alias confspec.fn fun(data: confspec.data, conf: any?): boolean

---@class (partial) confspec.data
M.data_fns={}

---@param msg string
---@return never
function M.data_fns:dev_error(msg)
  local indent=msg:match('^ *')
  msg=msg:gsub('^'..indent,''):gsub('\n'..indent,'\n'):gsub('^\n*',''):gsub('\n*$','')
  error(('Critical(developer) error while running confspec for plugin \'%s\':\n\n')
  :format(self.opts.plugin_name)
  ..msg)
end

---@overload fun(err:{severity:number,msg:string}): false
---@overload fun(severity:number,msg:string): false
---@nodiscard
function M.data_fns:error(severity,msg)
  if type(severity)=='table' then
    local err=severity
    msg=err.msg
    severity=err.severity
  end

  if severity>self.opts.err_severity then
    return false
  end

  local indent=msg:match('^ *')
  ---@type confspec.error
  local err={
    msg = msg:gsub('^'..indent,''):gsub('\n'..indent,'\n'):gsub('^\n*',''):gsub('\n*$',''),
    severity = severity,
  }
  table.insert(self.errors,err)

  return false
end

---@param conf any
---@return string
---@nodiscard
function M.data_fns.to_repr(conf)
  return type(conf)=='string' and ('%q'):format(conf) or tostring(conf)
end

---@param conf any
---@return string
---@nodiscard
function M.data_fns:traceback_with_conf(conf)
  return ('`%s` (with the value `%s`)'):format(self.traceback,self.to_repr(conf))
end

---@param spec confspec.spec
---@param values any[]
---@param types (type[])?
---@return never
function M.data_fns:dev_error_bad_spec(spec, values, types)
  local wants={}
  for _,want in ipairs(values) do
    local s=('the value `%s`'):format(self.to_repr(want))
    table.insert(wants,' * '..s)
  end
  for _,want in ipairs(types or {}) do
    local s=('the type `%s`'):format(want)
    table.insert(wants,' * '..s)
  end

  local indent=[[
  ]]

  return self:dev_error(([[
  The spec at `%s` should be either
  %s
  But got the spec `%s` with the type `%s`.
  ]]):format(self.traceback,table.concat(wants,'\n'..indent),self.to_repr(spec),type(spec)))
end

---@param conf any
---@param types type[]
---@param values any[]
---@return false
---@nodiscard
function M.data_fns:error_bad_type_or_val(conf, types, values)
  local msg
  if #values==0 and #types==1 then
    msg=([[
    The option %s should be the type `%s`, but got a `%s`.
    ]]):format(self:traceback_with_conf(conf),types[1],type(conf))
  else
    local wants={}
    for _,want in ipairs(values) do
      local v=self.to_repr(want)
      local s=('the value `%s`'):format(self.to_repr(v))
      table.insert(wants,' * '..s)
    end

    for _,want in ipairs(types) do
      local s=('the type `%s`'):format(want)
      table.insert(wants,' * '..s)
    end

    local indent=[[
    ]]

    msg=([[
    The option `%s` should be either
    %s
    But got the value `%s` with the type `%s`.
    ]]):format(self.traceback,table.concat(wants,'\n'..indent),self.to_repr(conf),type(conf))
  end

  return self:error{
    severity=1,
    msg=msg,
  }
end

---@return false
---@nodiscard
function M.data_fns:error_need_set()
  return self:error{
    severity=1,
    msg=([[
    The option `%s` should be set, but is not.
    ]]):format(self.traceback)
  }
end

---@return false
---@nodiscard
function M.data_fns:error_not_list()
  return self:error{
    severity = 2,
    msg = ([[
    The option `%s` should be a list (see `:help vim.islist()`).
    ]]):format(self.traceback)
  }
end

---@see https://en.wikipedia.org/wiki/Levenshtein_distance
local function levenshtein(str1,str2)
    local substitution_cost=function (a,b)
        if a==b then return 0 end
        if string.lower(a)==string.lower(b) then return 1 end
        return 2
    end
    local d={}
    for i=0,#str1 do
        d[i]={[0]=i}
    end
    for i=0,#str2 do
        d[0][i]=i
    end
    for i=1,#str1 do
        for j=1,#str2 do
            d[i][j]=math.min(d[i-1][j]+2,
                d[i][j-1]+2,
                d[i-1][j-1]+substitution_cost(str1:sub(i,i),str2:sub(j,j)))
        end
    end
    return d[#str1][#str2]
end

---@param idx any
---@param valids table
---@return false
---@nodiscard
function M.data_fns:error_dont_set(idx, valids)
  local suggestion=''

  local possible={}
  for _,optname in ipairs(type(idx)=='string' and valids or {}) do
    if type(optname)=='string' then
      ---Numbers and limit from python name-error suggestion algorithm
      ---@see https://docs.python.org/3.10/whatsnew/3.10.html#nameerrors
      local limit=(#(idx)+#optname+3)*2/6
      if levenshtein(optname,idx)<limit then
        table.insert(possible,optname)
      end
    end
  end

  if #possible>0 then
    suggestion=('Did you mean `%s`?'):format(table.concat(possible,'`, `'))
  end

  return self:error{
    severity = 2,
    msg = ([[
    The option `%s` is set, but it should not be set.

    %s
    ]]):format(self:merge_traceback(idx),suggestion)
  }
end

---@param fn function
---@param nparams_min number
---@param nparams_max number
---@return boolean
---@nodiscard
function M.data_fns:error_bad_fn_n_params(fn,nparams_min,nparams_max)
  local tonum={
    [0]='zero',
    [1]='one',
    [2]='two',
    [3]='three',
    [4]='four',
    [5]='five',
    [6]='six',
    [7]='seven',
    [8]='eight',
    [9]='nine',
  }
  local min_name=tonum[nparams_min] or tostring(nparams_min)
  local max_name=tonum[nparams_max] or tostring(nparams_max)
  local info=debug.getinfo(fn,'u')

  local msg
  if min_name==max_name then
    msg=([[
    The option `%s` is a function which should take %s and only %s argument%s.
    It currently takes %d%s argument%s.
    ]]):format(self.traceback,min_name,min_name,nparams_min~=1 and 's' or '',info.nparams,
    info.isvararg and ' or more' or '',(info.nparams~=1 or info.isvararg) and 's' or '')
  else
    msg=([[
    The option `%s` is a function which should take %s to %s arguments.
    It currently takes %d%s argument%s.
    ]]):format(self.traceback,min_name,max_name,info.nparams,
    info.isvararg and ' or more' or '',(info.nparams~=1 or info.isvararg) and 's' or '')
  end

  return self:error{
    severity = 3,
    msg = msg,
  }
end

---@param conf any
---@param msg string
---@param severity number
---@return boolean
---@nodiscard
function M.data_fns:error_not_detected(conf,msg,severity)
  return self:error{
    severity = severity,
    msg = ([[
    The option %s is not detected as a %s.
    ]]):format(self:traceback_with_conf(conf),msg),
  }
end

---@param conf any
---@param types type[]|type?
---@param values (any[])?
---@return boolean
---@nodiscard
function M.data_fns:assert_is(conf, types, values)
  for _,v in ipairs(values or {}) do
    if conf==v then return true end
  end

  types=(type(types)=='table' and types or {types})
  for _,v in ipairs(types) do
    if type(conf)==v then return true end
  end

  if #types==0 and #(values or {})==0 then
    self:dev_error[[
    Length of both {types} and {values} is zero; in function `self:assert_is`
    ]]
  end

  if conf==nil then
    return self:error_need_set()
  else
    return self:error_bad_type_or_val(conf, types, values or {})
  end
end

---@param conf any
---@return boolean
---@nodiscard
function M.data_fns:assert_is_list(conf)
  if not self:assert_is(conf, 'table') then
    return false
  end

  if vim.islist(conf) then
    return true
  end

  return self:error_not_list()
end

---@param conf function
---@param nparams number
---@param nparams_max number?
---@return boolean
---@nodiscard
function M.data_fns:assert_n_params(conf,nparams,nparams_max)
    nparams_max=nparams_max or nparams
    local info=debug.getinfo(conf,'u')
    if info.nparams<=nparams_max
        and (info.nparams>=nparams or info.isvararg) then
        return true
    end

    return self:error_bad_fn_n_params(conf,nparams,nparams_max)
end

---@generic T
---@param list (T|any)[]
---@param value T
---@return boolean
function in_list(list,value)
    for _,v in ipairs(list) do
        if v==value then return true end
    end
    return false
end

---@param conf any
---@param other_filetypes string[]
---@return boolean
---@nodiscard
function M.data_fns:assert_filetype(conf,other_filetypes)
  if not self:assert_is(conf,'string') then
    return false
  end

  if vim.treesitter.language.add(vim.treesitter.language.get_lang(conf) or '') then
    return true
  elseif in_list(vim.fn.getcompletion('','filetype'),conf) then
    return true
  elseif in_list(other_filetypes,conf) then
    return true
  else
    return self:error_not_detected(conf,'filetype',3)
  end
end

---@param conf any
---@return boolean
---@nodiscard
function M.data_fns:assert_tslang(conf)
  if not self:assert_is(conf,'string') then
    return false
  end

  if vim.treesitter.language.add(conf) then
    return true
  else
    return self:error_not_detected(conf,'treesitter language',2)
  end
end

---@param conf any
---@param lang string
---@return boolean
---@nodiscard
function M.data_fns:assert_tsnode_in_tslang(conf,lang)
  if not self:assert_is(conf,'string') then
    return false
  end

  if not vim.treesitter.language.add(lang) then
    self:dev_error[[
    {lang} is not a valid tslang; in function `self:assert_tsnode_in_tslang`
    (Make sure to run {lang} trough `self:assert_tslang` first)
    ]]
  end

  if conf:match('^[-a-zA-Z0-9_][-a-zA-Z0-9._]*$')
    and pcall(vim.treesitter.query.parse,lang,('(%s)'):format(conf)) then
    return true
  else
    return self:error_not_detected(conf,'valid TSNode type for language '..lang,1)
  end
end

---@param spec confspec.spec
---@param conf any
---@return boolean
---@nodiscard
function M.data_fns:run_spec(spec, conf)
  return self.meta_spec.default_handler(self,conf,spec)
end

---@param index any
---@param is_key boolean?
---@return string
---@nodiscard
function M.data_fns:merge_traceback(index,is_key)
    if is_key then
        return self.traceback..'#index['..self.to_repr(index)..']'
    end

    if type(index)~='string' then
        return self.traceback..'['..self.to_repr(index)..']'
    elseif self.traceback=='' then
        return index
    else
        return self.traceback..'.'..index
    end
end

---@param spec confspec.spec
---@param index any
---@param tbl table
---@return boolean
function M.data_fns:index_run_spec(spec, index, tbl)
  local data=setmetatable({
    traceback=self:merge_traceback(index)
  },{__index=self})

  return data:run_spec(spec, tbl[index])
end

---@param spec confspec.spec
---@param index any
---@return boolean
function M.data_fns:index_key_run_spec(spec, index)
  local data=setmetatable({
    traceback=self:merge_traceback(index, true)
  },{__index=self})

  return data:run_spec(spec, index)
end

---@class confspec.c
---@field [string] confspec.fn
M.c={}

function M.c.all(_data, _conf)
  return true
end
function M.c.any_nonnil(data, conf)
  return data:assert_is(conf, {'number', 'string', 'boolean', 'table' ,'function', 'thread', 'userdata'})
end
function M.c.number(data, conf)
  return data:assert_is(conf, 'number')
end
function M.c.string(data, conf)
  return data:assert_is(conf, 'string')
end
function M.c.boolean(data, conf)
  return data:assert_is(conf, 'boolean')
end
function M.c.table(data, conf)
  return data:assert_is(conf, 'table')
end
function M.c.is_function(data, conf)
  return data:assert_is(conf, 'function')
end
function M.c.is_tslang(data, conf)
  return data:assert_tslang(conf)
end

---@class confspec.g
---@field [string] fun(...): confspec.spec
M.g={}

---@param types type[]|type?
---@param values (any[])?
---@return confspec.fn
function M.g.type_or_value(types,values)
  ---@param data confspec.data
  return function(data, conf)
    return data:assert_is(conf, types, values)
  end
end

---@param values any[]
---@return confspec.fn
function M.g.enum(values)
  return M.g.type_or_value(nil, values)
end

---@param type_ type[]|type
---@return confspec.fn
function M.g.type(type_)
  return M.g.type_or_value(type_)
end

---@param nparams number
---@param nparams_max number?
function M.g.func_n_params(nparams,nparams_max)
  ---@param data confspec.data
  return function(data, conf)
    if not data:assert_is(conf, 'function') then
      return false
    end

    return data:assert_n_params(conf, nparams, nparams_max)
  end
end

---@param other_filetypes string[]
function M.g.is_filetype(other_filetypes)
  ---@param data confspec.data
  return function(data, conf)
    return data:assert_filetype(conf, other_filetypes)
  end
end

---@class confspec.d
---@field [string] fun(spec: confspec.spec, ...): confspec.spec
M.d={}

function M.d.list(spec)
  ---@param data confspec.data
  return function(data, conf)
    if not data:assert_is_list(conf) then
      return false
    end

    for k in ipairs(conf) do
      data:index_run_spec(spec,k,conf)
    end

    return true
  end
end

---@param spec confspec.spec
function M.d.table(key_spec, spec)
  ---@param data confspec.data
  return function(data, conf)
    if not data:assert_is(conf,'table') then
      return false
    end

    for k in pairs(conf) do
      if data:index_key_run_spec(key_spec,k) then
        data:index_run_spec(spec,k,conf)
      end
    end

    return true
  end
end

---@param fn fun(data: confspec.data, conf: any?, key: any): boolean
function M.d.table_fn_with_key(key_spec, fn)
  ---@param data confspec.data
  return function(data, conf)
    if not data:assert_is(conf,'table') then
      return false
    end

    for k in pairs(conf) do
      if data:index_key_run_spec(key_spec,k) then
        data:index_run_spec(function(data_, conf_)
          fn(data_, conf_, k)
        end,k,conf)
      end
    end

    return true
  end
end

---@param spec confspec.spec
---@return confspec.fn
function M.d.opt(spec)
  ---@param data confspec.data
  return function (data, conf)
    if conf==nil then
      return true
    end

    return data:run_spec(spec, conf)
  end
end

---@param spec confspec.spec
---@return confspec.fn
function M.d.type_or_table(spec, type_)
  ---@param data confspec.data
  return function (data, conf)
    if not data:assert_is(conf, {type_, 'table'}) then
      return false
    end

    if type(conf)==type_ then
      return true
    end

    return data:run_spec(spec, conf)
  end
end

---@class confspec.meta_spec
---@field type_handler table<type,confspec.handler_fn?>
---@field string_handler table<string,confspec.handler_fn?>
---@field default_handler confspec.handler_fn
---@field default boolean?

---@type confspec.meta_spec
local meta_spec_default={
  ---@enum (key) confspec.meta_spec.default.string_handler
  string_handler={
    nonnil=M.c.any_nonnil,
    boolean=M.c.boolean,
    string=M.c.string,
    number=M.c.number,
    table=M.c.table,
    ['function']=M.c.is_function,
    all=M.c.all,
    ['boolean?']=M.d.opt(M.c.boolean),
    ['string?']=M.d.opt(M.c.string),
    ['number?']=M.d.opt(M.c.number),
    ['function?']=M.d.opt(M.c.is_function),
    ['table?']=M.d.opt(M.c.table),
  },

  ---@enum (key) confspec.meta_spec.default.type_handler
  type_handler={
    ---@param data confspec.data
    string=function (data,conf,spec)
      if data.meta_spec.string_handler[spec] then
        return data.meta_spec.string_handler[spec --[[@as string]]](data, conf, spec)
      end

      data:dev_error_bad_spec(spec, vim.tbl_keys(data.meta_spec.string_handler))
    end,

    ['function']=function (data,conf,spec)
      return spec(data,conf)
    end,

    ---@param data confspec.data
    ---@return boolean
    table=function (data,conf,spec)
      if not data:assert_is(conf,'table') then
        return false
      end

      local keys={}
      for idx in pairs(conf) do
        keys[idx]=true

        if not spec[idx] then
          local _=data:error_dont_set(idx,vim.tbl_keys(spec))
        else
          data:index_run_spec(spec[idx],idx,conf)
        end
      end

      for idx in pairs(spec) do
        if keys[idx] then
        else
          data:index_run_spec(spec[idx],idx,conf)
        end
      end

      return true
    end,
  },

  default_handler=function (data, conf, spec)
    if data.meta_spec.type_handler[type(spec)] then
      return data.meta_spec.type_handler[type(spec)](data, conf, spec)
    end

    data:dev_error_bad_spec(spec, {}, vim.tbl_keys(data.meta_spec.type_handler))
    return false
  end,
}

---@param spec confspec.spec
---@param conf any?
---@param opts confspec.opts
---@param meta_spec confspec.meta_spec?
function M.validate(spec, conf, opts, meta_spec)
  meta_spec=vim.tbl_extend('force',meta_spec_default,meta_spec or {}) --[[@as confspec.meta_spec]]

  ---@type confspec.data
  local data=setmetatable({
    meta_spec=meta_spec,
    opts=opts,
    traceback='',
    errors={},
  },{__index = M.data_fns})

  local _=data:run_spec(spec, conf)

  if #data.errors>0 then
    M.errors_error(opts, data.errors)
  end
end

---@param conf any?
---@param plugin_name string
---@param default {err_severity:number?,err_format:confspec.err_format?}?
---@param overwrite {err_severity:number?,err_format:confspec.err_format?}?
---@return confspec.opts
function M.opts_generate(conf,plugin_name,default,overwrite)
  default=default or {}
  overwrite=overwrite or {}
  conf=type(conf)=='table' and conf or {}

  local function get(idx, def)
    return overwrite[idx] or (conf[idx]==nil and (default[idx] or def)) or conf[idx]
  end

  local err_format=get('err_format','default')
  if err_format~='default' and  err_format~='borderless' and err_format~='table' and err_format~='raw' and err_format~='minimal' then
    error(plugin_name..': option `.err_format` needs to be one of "default", "raw", "borderless", "minimal" or "table"')
  end

  local err_severity=get('err_severity',2)
  if type(err_severity)~='number' then
    error(plugin_name..': option `.err_severity` needs to be number or nil')
  end

  return {
    plugin_name=plugin_name,
    err_severity=err_severity,
    err_format=err_format,
  }
end

---@param opts confspec.opts
---@param errors confspec.error[]
---@return never
function M.errors_error(opts, errors)
  if opts.err_format=='table' then
    error(errors)
  elseif opts.err_format=='raw' then
    error(vim.inspect(errors))
  elseif opts.err_format=='minimal' then
    error(('For plugin `%s`: validation of config failed'):format(opts.plugin_name))
  end

  local has_border=opts.err_format~='borderless'

  local msg={}

  local max_severity=math.huge

  for _, err in ipairs(errors) do
    if err.severity<max_severity then
      max_severity=err.severity
    end

    if has_border then
      table.insert(msg,err.severity)
    end
    vim.list_extend(msg,vim.split(err.msg,'\n'))
    table.insert(msg,'')
  end

  local top=('Configuration for the plugin \'%s\' is%s incorrect:')
  :format(opts.plugin_name,max_severity>=2 and ' POSSIBLY' or '')

  table.insert(msg,1,'')
  table.insert(msg,1,top)

  if has_border then
    local max_len=0
    for _,v in ipairs(msg) do
      if type(v)=='string' then
        max_len=math.max(max_len,#v)
      end
    end

    for k,v in ipairs(msg) do
      if type(v)=='number' then
        local severity_msg=v<=1 and 'high' or v==2 and 'medium' or 'low'

        msg[k]=('------ Severity: %s(%d) %s'):format(severity_msg,v,('-'):rep(max_len)):sub(1,math.min(max_len,vim.o.columns-1))
      end
    end

    local border=('-'):rep(math.min(max_len,vim.o.columns-1))
    table.insert(msg,1,border)
    table.insert(msg,border)
  else
    assert(table.remove(msg)=='')
  end
  error('\n\n\n'..table.concat(msg,'\n')..'\n\n')
end

---@alias confspec.merge.spec.nonnil boolean|number|userdata|thread|table|function
---@alias confspec.merge.spec.fn fun(val: confspec.merge.spec.nonnil, def: any):any
---@alias confspec.merge.spec
---| {[any]: confspec.merge.spec}
---| confspec.merge.spec.fn
---| false

---@alias confspec.merge.specT<T> {[P in keyof T]: confspec.merge.spec;}

---@generic T:confspec.merge.spec.nonnil, G
---@param conf T
---@param default G
---@return T|G
function M.merge_listish_overwrite(conf, default)
  assert(type(default)=='table')

  if type(conf)~='table' then
    return conf
  end

  local out={}
  for k,v in pairs(default) do
    -- What if {conf} has number indexes, but not index 1
    -- This is a feature, not a bug: if the user wants the default list
    -- to not be overwritten, only extended, they can `{nil,...}`
    if type(k)=='number' and conf[1] then
    else
      out[k]=v
    end
  end
  for k,v in pairs(conf) do
    if type(k)=='number' then
      table.insert(out,v)
    else
      out[k]=v
    end
  end
  return out
end

---@generic T,G
---@param spec confspec.merge.spec
---@param conf T|G
---@param default T
---@return std.NotNull<G>|T
function M.merge(spec, conf, default)
  assert((type(default)=='table')==(type(spec)=='table') or type(spec)=='function')

  if conf==nil then
    return default
  elseif type(spec)=='function' then
    return spec(conf, default)
  elseif spec==false then
    return conf
  elseif type(conf)~='table' then
    return conf
  end

  local out={}
  for k,v in pairs(default) do
    out[k]=v
  end
  for k,v in pairs(conf) do
    if spec[k] then
      out[k]=M.merge(spec[k],v,default[k])
    else
      out[k]=v
    end
  end
  return out
end

---@generic T,G
---@param spec confspec.merge.spec
---@param conf_ G
---@param default_ T
---@return std.NotNull<G>|T
function M.merge_with_default_opt(spec, conf_, default_)
  return M.merge(function(conf,default)
    if type(conf)~='table' or type(default)~='table' then
      return conf
    elseif conf.default==false then
      return conf
    end

    return M.merge(spec,conf,default)
  end, conf_, default_)
end

---@alias confspec.mspec
---|table<any,confspec.mspec>
---|{["_"]:confspec.spec,[1]:any,[2]:'list'?}
---|any

---@alias confspec.mspecT<T> {[P in keyof T]: confspec.mspec;}

---@param mspec confspec.mspec
---@param id any?
---@return any
function M.mspec_to_default(mspec,id)
  if type(mspec)~='table' then
    return mspec
  elseif mspec[id or '_'] then
    return mspec[1]
  end

  local out={}
  for k,v in pairs(mspec --[[@as table]]) do
    out[k]=M.mspec_to_default(v,id)
  end
  return out
end

---@param mspec confspec.mspec
---@param id any?
---@return confspec.merge.spec
function M.mspec_to_merge_spec(mspec,id)
  if type(mspec)~='table' then
    return false
  elseif mspec[id or '_'] then
    return mspec[2]=='list' and M.merge_listish_overwrite or false
  end

  local out={}
  for k,v in pairs(mspec --[[@as table]]) do
    out[k]=M.mspec_to_merge_spec(v,id)
  end
  return out
end

---@param mspec confspec.mspec
---@param id any?
---@return confspec.spec
function M.mspec_to_spec(mspec,id)
  if type(mspec)~='table' then
    return M.g.type(type(mspec))
  elseif mspec[id or '_'] then
    return mspec[id or '_']
  end

  local out={}
  for k,v in pairs(mspec --[[@as table]]) do
    out[k]=M.mspec_to_spec(v,id)
  end
  return out
end

---@generic T
---@param mspec confspec.mspec
---@param conf T
---@param plugin_name string
---@param id any?
---@return T
function M.merge_and_validate(mspec, conf, plugin_name, id)
  id=id or '_'
  local default=M.mspec_to_default(mspec)
  local merge_spec=M.mspec_to_merge_spec(mspec)
  local spec=M.mspec_to_spec(mspec)

  conf=M.merge(merge_spec, conf, default)
  M.validate(spec,conf,M.opts_generate(conf,plugin_name))
  return conf
end

return M
