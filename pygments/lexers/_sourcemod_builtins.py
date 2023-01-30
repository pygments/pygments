"""
    pygments.lexers._sourcemod_builtins
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    This file contains the names of SourceMod functions.

    Do not edit the FUNCTIONS list by hand.

    Run with `python -I` to regenerate.

    :copyright: Copyright 2006-2022 by the Pygments team, see AUTHORS.
    :license: BSD, see LICENSE for details.
"""

FUNCTIONS = (
    'OnEntityCreated',
    'OnEntityDestroyed',
    'OnGetGameDescription',
    'OnLevelInit',
    'SDKHook',
    'SDKHookEx',
    'SDKUnhook',
    'SDKHooks_TakeDamage',
    'SDKHooks_DropWeapon',
    'TopMenuHandler',
    'CreateTopMenu',
    'LoadTopMenuConfig',
    'AddToTopMenu',
    'GetTopMenuInfoString',
    'GetTopMenuObjName',
    'RemoveFromTopMenu',
    'DisplayTopMenu',
    'DisplayTopMenuCategory',
    'FindTopMenuCategory',
    'SetTopMenuTitleCaching',
    'OnAdminMenuCreated',
    'OnAdminMenuReady',
    'GetAdminTopMenu',
    'AddTargetsToMenu',
    'AddTargetsToMenu2',
    'RedisplayAdminMenu',
    'TEHook',
    'AddTempEntHook',
    'RemoveTempEntHook',
    'TE_Start',
    'TE_IsValidProp',
    'TE_WriteNum',
    'TE_ReadNum',
    'TE_WriteFloat',
    'TE_ReadFloat',
    'TE_WriteVector',
    'TE_ReadVector',
    'TE_WriteAngles',
    'TE_WriteFloatArray',
    'TE_Send',
    'TE_WriteEncodedEnt',
    'TE_SendToAll',
    'TE_SendToClient',
    'CreateKeyValues',
    'KvSetString',
    'KvSetNum',
    'KvSetUInt64',
    'KvSetFloat',
    'KvSetColor',
    'KvSetVector',
    'KvGetString',
    'KvGetNum',
    'KvGetFloat',
    'KvGetColor',
    'KvGetUInt64',
    'KvGetVector',
    'KvJumpToKey',
    'KvJumpToKeySymbol',
    'KvGotoFirstSubKey',
    'KvGotoNextKey',
    'KvSavePosition',
    'KvDeleteKey',
    'KvDeleteThis',
    'KvGoBack',
    'KvRewind',
    'KvGetSectionName',
    'KvSetSectionName',
    'KvGetDataType',
    'KeyValuesToFile',
    'FileToKeyValues',
    'StringToKeyValues',
    'KvSetEscapeSequences',
    'KvNodesInStack',
    'KvCopySubkeys',
    'KvFindKeyById',
    'KvGetNameSymbol',
    'KvGetSectionSymbol',
    'TE_SetupSparks',
    'TE_SetupSmoke',
    'TE_SetupDust',
    'TE_SetupMuzzleFlash',
    'TE_SetupMetalSparks',
    'TE_SetupEnergySplash',
    'TE_SetupArmorRicochet',
    'TE_SetupGlowSprite',
    'TE_SetupExplosion',
    'TE_SetupBloodSprite',
    'TE_SetupBeamRingPoint',
    'TE_SetupBeamPoints',
    'TE_SetupBeamLaser',
    'TE_SetupBeamRing',
    'TE_SetupBeamFollow',
    'HookEvent',
    'HookEventEx',
    'UnhookEvent',
    'CreateEvent',
    'FireEvent',
    'CancelCreatedEvent',
    'GetEventBool',
    'SetEventBool',
    'GetEventInt',
    'SetEventInt',
    'GetEventFloat',
    'SetEventFloat',
    'GetEventString',
    'SetEventString',
    'GetEventName',
    'SetEventBroadcast',
    'GetUserMessageType',
    'GetUserMessageId',
    'GetUserMessageName',
    'StartMessage',
    'StartMessageEx',
    'EndMessage',
    'MsgHook',
    'MsgPostHook',
    'HookUserMessage',
    'UnhookUserMessage',
    'StartMessageAll',
    'StartMessageOne',
    'InactivateClient',
    'ReconnectClient',
    'GetMaxEntities',
    'GetEntityCount',
    'IsValidEntity',
    'IsValidEdict',
    'IsEntNetworkable',
    'CreateEdict',
    'RemoveEdict',
    'GetEdictFlags',
    'SetEdictFlags',
    'GetEdictClassname',
    'GetEntityNetClass',
    'ChangeEdictState',
    'GetEntData',
    'SetEntData',
    'GetEntDataFloat',
    'SetEntDataFloat',
    'GetEntDataEnt2',
    'SetEntDataEnt2',
    'GetEntDataVector',
    'SetEntDataVector',
    'GetEntDataString',
    'SetEntDataString',
    'FindSendPropOffs',
    'FindSendPropInfo',
    'FindDataMapOffs',
    'FindDataMapInfo',
    'GetEntSendPropOffs',
    'GetEntProp',
    'SetEntProp',
    'GetEntPropFloat',
    'SetEntPropFloat',
    'GetEntPropEnt',
    'SetEntPropEnt',
    'GetEntPropVector',
    'SetEntPropVector',
    'GetEntPropString',
    'SetEntPropString',
    'GetEntPropArraySize',
    'GetEntDataArray',
    'SetEntDataArray',
    'GetEntityAddress',
    'GetEntityClassname',
    'float',
    'FloatMul',
    'FloatDiv',
    'FloatAdd',
    'FloatSub',
    'FloatFraction',
    'RoundToZero',
    'RoundToCeil',
    'RoundToFloor',
    'RoundToNearest',
    'FloatCompare',
    'SquareRoot',
    'Pow',
    'Exponential',
    'Logarithm',
    'Sine',
    'Cosine',
    'Tangent',
    'FloatAbs',
    'ArcTangent',
    'ArcCosine',
    'ArcSine',
    'ArcTangent2',
    'RoundFloat',
    'operator%',
    'DegToRad',
    'RadToDeg',
    'GetURandomInt',
    'GetURandomFloat',
    'SetURandomSeed',
    'SetURandomSeedSimple',
    'RemovePlayerItem',
    'GivePlayerItem',
    'GetPlayerWeaponSlot',
    'IgniteEntity',
    'ExtinguishEntity',
    'TeleportEntity',
    'ForcePlayerSuicide',
    'SlapPlayer',
    'FindEntityByClassname',
    'GetClientEyeAngles',
    'CreateEntityByName',
    'DispatchSpawn',
    'DispatchKeyValue',
    'DispatchKeyValueFloat',
    'DispatchKeyValueVector',
    'GetClientAimTarget',
    'GetTeamCount',
    'GetTeamName',
    'GetTeamScore',
    'SetTeamScore',
    'GetTeamClientCount',
    'SetEntityModel',
    'GetPlayerDecalFile',
    'GetPlayerJingleFile',
    'GetServerNetStats',
    'EquipPlayerWeapon',
    'ActivateEntity',
    'SetClientInfo',
    'GivePlayerAmmo',
    'SetClientListeningFlags',
    'GetClientListeningFlags',
    'SetListenOverride',
    'GetListenOverride',
    'IsClientMuted',
    'TR_GetPointContents',
    'TR_GetPointContentsEnt',
    'TR_TraceRay',
    'TR_TraceHull',
    'TR_TraceRayFilter',
    'TR_TraceHullFilter',
    'TR_TraceRayEx',
    'TR_TraceHullEx',
    'TR_TraceRayFilterEx',
    'TR_TraceHullFilterEx',
    'TR_GetFraction',
    'TR_GetEndPosition',
    'TR_GetEntityIndex',
    'TR_DidHit',
    'TR_GetHitGroup',
    'TR_GetPlaneNormal',
    'TR_PointOutsideWorld',
    'SortIntegers',
    'SortFloats',
    'SortStrings',
    'SortFunc1D',
    'SortCustom1D',
    'SortCustom2D',
    'SortADTArray',
    'SortFuncADTArray',
    'SortADTArrayCustom',
    'CompileRegex',
    'MatchRegex',
    'GetRegexSubString',
    'SimpleRegexMatch',
    'TF2_GetPlayerClass',
    'TF2_SetPlayerClass',
    'TF2_RemoveWeaponSlot',
    'TF2_RemoveAllWeapons',
    'TF2_IsPlayerInCondition',
    'TF2_GetObjectType',
    'TF2_GetObjectMode',
    'NominateMap',
    'RemoveNominationByMap',
    'RemoveNominationByOwner',
    'GetExcludeMapList',
    'GetNominatedMapList',
    'CanMapChooserStartVote',
    'InitiateMapChooserVote',
    'HasEndOfMapVoteFinished',
    'EndOfMapVoteEnabled',
    'OnNominationRemoved',
    'OnMapVoteStarted',
    'CreateTimer',
    'KillTimer',
    'TriggerTimer',
    'GetTickedTime',
    'GetMapTimeLeft',
    'GetMapTimeLimit',
    'ExtendMapTimeLimit',
    'GetTickInterval',
    'OnMapTimeLeftChanged',
    'IsServerProcessing',
    'CreateDataTimer',
    'ByteCountToCells',
    'CreateArray',
    'ClearArray',
    'CloneArray',
    'ResizeArray',
    'GetArraySize',
    'PushArrayCell',
    'PushArrayString',
    'PushArrayArray',
    'GetArrayCell',
    'GetArrayString',
    'GetArrayArray',
    'SetArrayCell',
    'SetArrayString',
    'SetArrayArray',
    'ShiftArrayUp',
    'RemoveFromArray',
    'SwapArrayItems',
    'FindStringInArray',
    'FindValueInArray',
    'ProcessTargetString',
    'ReplyToTargetError',
    'MultiTargetFilter',
    'AddMultiTargetFilter',
    'RemoveMultiTargetFilter',
    'OnBanClient',
    'OnBanIdentity',
    'OnRemoveBan',
    'BanClient',
    'BanIdentity',
    'RemoveBan',
    'CreateTrie',
    'SetTrieValue',
    'SetTrieArray',
    'SetTrieString',
    'GetTrieValue',
    'GetTrieArray',
    'GetTrieString',
    'RemoveFromTrie',
    'ClearTrie',
    'GetTrieSize',
    'GetFunctionByName',
    'CreateGlobalForward',
    'CreateForward',
    'GetForwardFunctionCount',
    'AddToForward',
    'RemoveFromForward',
    'RemoveAllFromForward',
    'Call_StartForward',
    'Call_StartFunction',
    'Call_PushCell',
    'Call_PushCellRef',
    'Call_PushFloat',
    'Call_PushFloatRef',
    'Call_PushArray',
    'Call_PushArrayEx',
    'Call_PushString',
    'Call_PushStringEx',
    'Call_Finish',
    'Call_Cancel',
    'NativeCall',
    'CreateNative',
    'ThrowNativeError',
    'GetNativeStringLength',
    'GetNativeString',
    'SetNativeString',
    'GetNativeCell',
    'GetNativeCellRef',
    'SetNativeCellRef',
    'GetNativeArray',
    'SetNativeArray',
    'FormatNativeString',
    'RequestFrameCallback',
    'RequestFrame',
    'OnRebuildAdminCache',
    'DumpAdminCache',
    'AddCommandOverride',
    'GetCommandOverride',
    'UnsetCommandOverride',
    'CreateAdmGroup',
    'FindAdmGroup',
    'SetAdmGroupAddFlag',
    'GetAdmGroupAddFlag',
    'GetAdmGroupAddFlags',
    'SetAdmGroupImmuneFrom',
    'GetAdmGroupImmuneCount',
    'GetAdmGroupImmuneFrom',
    'AddAdmGroupCmdOverride',
    'GetAdmGroupCmdOverride',
    'RegisterAuthIdentType',
    'CreateAdmin',
    'GetAdminUsername',
    'BindAdminIdentity',
    'SetAdminFlag',
    'GetAdminFlag',
    'GetAdminFlags',
    'AdminInheritGroup',
    'GetAdminGroupCount',
    'GetAdminGroup',
    'SetAdminPassword',
    'GetAdminPassword',
    'FindAdminByIdentity',
    'RemoveAdmin',
    'FlagBitsToBitArray',
    'FlagBitArrayToBits',
    'FlagArrayToBits',
    'FlagBitsToArray',
    'FindFlagByName',
    'FindFlagByChar',
    'FindFlagChar',
    'ReadFlagString',
    'CanAdminTarget',
    'CreateAuthMethod',
    'SetAdmGroupImmunityLevel',
    'GetAdmGroupImmunityLevel',
    'SetAdminImmunityLevel',
    'GetAdminImmunityLevel',
    'FlagToBit',
    'BitToFlag',
    'ServerCommand',
    'ServerCommandEx',
    'InsertServerCommand',
    'ServerExecute',
    'ClientCommand',
    'FakeClientCommand',
    'FakeClientCommandEx',
    'PrintToServer',
    'PrintToConsole',
    'ReplyToCommand',
    'GetCmdReplySource',
    'SetCmdReplySource',
    'IsChatTrigger',
    'ShowActivity2',
    'ShowActivity',
    'ShowActivityEx',
    'FormatActivitySource',
    'SrvCmd',
    'RegServerCmd',
    'ConCmd',
    'RegConsoleCmd',
    'RegAdminCmd',
    'GetCmdArgs',
    'GetCmdArg',
    'GetCmdArgString',
    'CreateConVar',
    'FindConVar',
    'ConVarChanged',
    'HookConVarChange',
    'UnhookConVarChange',
    'GetConVarBool',
    'SetConVarBool',
    'GetConVarInt',
    'SetConVarInt',
    'GetConVarFloat',
    'SetConVarFloat',
    'GetConVarString',
    'SetConVarString',
    'ResetConVar',
    'GetConVarDefault',
    'GetConVarFlags',
    'SetConVarFlags',
    'GetConVarBounds',
    'SetConVarBounds',
    'GetConVarName',
    'QueryClientConVar',
    'GetCommandIterator',
    'ReadCommandIterator',
    'CheckCommandAccess',
    'CheckAccess',
    'IsValidConVarChar',
    'GetCommandFlags',
    'SetCommandFlags',
    'FindFirstConCommand',
    'FindNextConCommand',
    'SendConVarValue',
    'AddServerTag',
    'RemoveServerTag',
    'CommandListener',
    'AddCommandListener',
    'RemoveCommandListener',
    'CommandExists',
    'OnClientSayCommand',
    'OnClientSayCommand_Post',
    'TF2_IgnitePlayer',
    'TF2_RespawnPlayer',
    'TF2_RegeneratePlayer',
    'TF2_AddCondition',
    'TF2_RemoveCondition',
    'TF2_SetPlayerPowerPlay',
    'TF2_DisguisePlayer',
    'TF2_RemovePlayerDisguise',
    'TF2_StunPlayer',
    'TF2_MakeBleed',
    'TF2_GetClass',
    'TF2_CalcIsAttackCritical',
    'TF2_OnIsHolidayActive',
    'TF2_IsHolidayActive',
    'TF2_IsPlayerInDuel',
    'TF2_RemoveWearable',
    'TF2_OnConditionAdded',
    'TF2_OnConditionRemoved',
    'TF2_OnWaitingForPlayersStart',
    'TF2_OnWaitingForPlayersEnd',
    'TF2_OnPlayerTeleport',
    'SQL_Connect',
    'SQL_DefConnect',
    'SQL_ConnectCustom',
    'SQLite_UseDatabase',
    'SQL_CheckConfig',
    'SQL_GetDriver',
    'SQL_ReadDriver',
    'SQL_GetDriverIdent',
    'SQL_GetDriverProduct',
    'SQL_SetCharset',
    'SQL_GetAffectedRows',
    'SQL_GetInsertId',
    'SQL_GetError',
    'SQL_EscapeString',
    'SQL_QuoteString',
    'SQL_FastQuery',
    'SQL_Query',
    'SQL_PrepareQuery',
    'SQL_FetchMoreResults',
    'SQL_HasResultSet',
    'SQL_GetRowCount',
    'SQL_GetFieldCount',
    'SQL_FieldNumToName',
    'SQL_FieldNameToNum',
    'SQL_FetchRow',
    'SQL_MoreRows',
    'SQL_Rewind',
    'SQL_FetchString',
    'SQL_FetchFloat',
    'SQL_FetchInt',
    'SQL_IsFieldNull',
    'SQL_FetchSize',
    'SQL_BindParamInt',
    'SQL_BindParamFloat',
    'SQL_BindParamString',
    'SQL_Execute',
    'SQL_LockDatabase',
    'SQL_UnlockDatabase',
    'SQLTCallback',
    'SQL_IsSameConnection',
    'SQL_TConnect',
    'SQL_TQuery',
    'SQL_CreateTransaction',
    'SQL_AddQuery',
    'SQLTxnSuccess',
    'SQLTxnFailure',
    'SQL_ExecuteTransaction',
    'CloseHandle',
    'CloneHandle',
    'MenuHandler',
    'CreateMenu',
    'DisplayMenu',
    'DisplayMenuAtItem',
    'AddMenuItem',
    'InsertMenuItem',
    'RemoveMenuItem',
    'RemoveAllMenuItems',
    'GetMenuItem',
    'GetMenuSelectionPosition',
    'GetMenuItemCount',
    'SetMenuPagination',
    'GetMenuPagination',
    'GetMenuStyle',
    'SetMenuTitle',
    'GetMenuTitle',
    'CreatePanelFromMenu',
    'GetMenuExitButton',
    'SetMenuExitButton',
    'GetMenuExitBackButton',
    'SetMenuExitBackButton',
    'SetMenuNoVoteButton',
    'CancelMenu',
    'GetMenuOptionFlags',
    'SetMenuOptionFlags',
    'IsVoteInProgress',
    'CancelVote',
    'VoteMenu',
    'VoteMenuToAll',
    'VoteHandler',
    'SetVoteResultCallback',
    'CheckVoteDelay',
    'IsClientInVotePool',
    'RedrawClientVoteMenu',
    'GetMenuStyleHandle',
    'CreatePanel',
    'CreateMenuEx',
    'GetClientMenu',
    'CancelClientMenu',
    'GetMaxPageItems',
    'GetPanelStyle',
    'SetPanelTitle',
    'DrawPanelItem',
    'DrawPanelText',
    'CanPanelDrawFlags',
    'SetPanelKeys',
    'SendPanelToClient',
    'GetPanelTextRemaining',
    'GetPanelCurrentKey',
    'SetPanelCurrentKey',
    'RedrawMenuItem',
    'InternalShowMenu',
    'GetMenuVoteInfo',
    'IsNewVoteAllowed',
    'PrefetchSound',
    'EmitAmbientSound',
    'FadeClientVolume',
    'StopSound',
    'EmitSound',
    'EmitSentence',
    'GetDistGainFromSoundLevel',
    'AmbientSHook',
    'NormalSHook',
    'AddAmbientSoundHook',
    'AddNormalSoundHook',
    'RemoveAmbientSoundHook',
    'RemoveNormalSoundHook',
    'EmitSoundToClient',
    'EmitSoundToAll',
    'ATTN_TO_SNDLEVEL',
    'GetGameSoundParams',
    'EmitGameSound',
    'EmitAmbientGameSound',
    'EmitGameSoundToClient',
    'EmitGameSoundToAll',
    'PrecacheScriptSound',
    'strlen',
    'StrContains',
    'strcmp',
    'strncmp',
    'StrEqual',
    'strcopy',
    'Format',
    'FormatEx',
    'VFormat',
    'StringToInt',
    'StringToIntEx',
    'IntToString',
    'StringToFloat',
    'StringToFloatEx',
    'FloatToString',
    'BreakString',
    'TrimString',
    'SplitString',
    'ReplaceString',
    'ReplaceStringEx',
    'GetCharBytes',
    'IsCharAlpha',
    'IsCharNumeric',
    'IsCharSpace',
    'IsCharMB',
    'IsCharUpper',
    'IsCharLower',
    'StripQuotes',
    'CharToUpper',
    'CharToLower',
    'FindCharInString',
    'StrCat',
    'ExplodeString',
    'ImplodeStrings',
    'GetVectorLength',
    'GetVectorDistance',
    'GetVectorDotProduct',
    'GetVectorCrossProduct',
    'NormalizeVector',
    'GetAngleVectors',
    'GetVectorAngles',
    'GetVectorVectors',
    'AddVectors',
    'SubtractVectors',
    'ScaleVector',
    'NegateVector',
    'MakeVectorFromPoints',
    'BaseComm_IsClientGagged',
    'BaseComm_IsClientMuted',
    'BaseComm_SetClientGag',
    'BaseComm_SetClientMute',
    'FormatUserLogText',
    'FindPluginByFile',
    'FindTarget',
    'AcceptEntityInput',
    'SetVariantBool',
    'SetVariantString',
    'SetVariantInt',
    'SetVariantFloat',
    'SetVariantVector3D',
    'SetVariantPosVector3D',
    'SetVariantColor',
    'SetVariantEntity',
    'GameRules_GetProp',
    'GameRules_SetProp',
    'GameRules_GetPropFloat',
    'GameRules_SetPropFloat',
    'GameRules_GetPropEnt',
    'GameRules_SetPropEnt',
    'GameRules_GetPropVector',
    'GameRules_SetPropVector',
    'GameRules_GetPropString',
    'GameRules_SetPropString',
    'GameRules_GetRoundState',
    'OnClientConnect',
    'OnClientConnected',
    'OnClientPutInServer',
    'OnClientDisconnect',
    'OnClientDisconnect_Post',
    'OnClientCommand',
    'OnClientSettingsChanged',
    'OnClientAuthorized',
    'OnClientPreAdminCheck',
    'OnClientPostAdminFilter',
    'OnClientPostAdminCheck',
    'GetMaxClients',
    'GetMaxHumanPlayers',
    'GetClientCount',
    'GetClientName',
    'GetClientIP',
    'GetClientAuthString',
    'GetClientAuthId',
    'GetSteamAccountID',
    'GetClientUserId',
    'IsClientConnected',
    'IsClientInGame',
    'IsClientInKickQueue',
    'IsClientAuthorized',
    'IsFakeClient',
    'IsClientSourceTV',
    'IsClientReplay',
    'IsClientObserver',
    'IsPlayerAlive',
    'GetClientInfo',
    'GetClientTeam',
    'SetUserAdmin',
    'GetUserAdmin',
    'AddUserFlags',
    'RemoveUserFlags',
    'SetUserFlagBits',
    'GetUserFlagBits',
    'CanUserTarget',
    'RunAdminCacheChecks',
    'NotifyPostAdminCheck',
    'CreateFakeClient',
    'SetFakeClientConVar',
    'GetClientHealth',
    'GetClientModel',
    'GetClientWeapon',
    'GetClientMaxs',
    'GetClientMins',
    'GetClientAbsAngles',
    'GetClientAbsOrigin',
    'GetClientArmor',
    'GetClientDeaths',
    'GetClientFrags',
    'GetClientDataRate',
    'IsClientTimingOut',
    'GetClientTime',
    'GetClientLatency',
    'GetClientAvgLatency',
    'GetClientAvgLoss',
    'GetClientAvgChoke',
    'GetClientAvgData',
    'GetClientAvgPackets',
    'GetClientOfUserId',
    'KickClient',
    'KickClientEx',
    'ChangeClientTeam',
    'GetClientSerial',
    'GetClientFromSerial',
    'FindStringTable',
    'GetNumStringTables',
    'GetStringTableNumStrings',
    'GetStringTableMaxStrings',
    'GetStringTableName',
    'FindStringIndex',
    'ReadStringTable',
    'GetStringTableDataLength',
    'GetStringTableData',
    'SetStringTableData',
    'AddToStringTable',
    'LockStringTables',
    'AddFileToDownloadsTable',
    'GetEntityFlags',
    'SetEntityFlags',
    'GetEntityMoveType',
    'SetEntityMoveType',
    'GetEntityRenderMode',
    'SetEntityRenderMode',
    'GetEntityRenderFx',
    'SetEntityRenderFx',
    'SetEntityRenderColor',
    'GetEntityGravity',
    'SetEntityGravity',
    'SetEntityHealth',
    'GetClientButtons',
    'EntityOutput',
    'HookEntityOutput',
    'UnhookEntityOutput',
    'HookSingleEntityOutput',
    'UnhookSingleEntityOutput',
    'SMC_CreateParser',
    'SMC_ParseFile',
    'SMC_GetErrorString',
    'SMC_ParseStart',
    'SMC_SetParseStart',
    'SMC_ParseEnd',
    'SMC_SetParseEnd',
    'SMC_NewSection',
    'SMC_KeyValue',
    'SMC_EndSection',
    'SMC_SetReaders',
    'SMC_RawLine',
    'SMC_SetRawLine',
    'BfWriteBool',
    'BfWriteByte',
    'BfWriteChar',
    'BfWriteShort',
    'BfWriteWord',
    'BfWriteNum',
    'BfWriteFloat',
    'BfWriteString',
    'BfWriteEntity',
    'BfWriteAngle',
    'BfWriteCoord',
    'BfWriteVecCoord',
    'BfWriteVecNormal',
    'BfWriteAngles',
    'BfReadBool',
    'BfReadByte',
    'BfReadChar',
    'BfReadShort',
    'BfReadWord',
    'BfReadNum',
    'BfReadFloat',
    'BfReadString',
    'BfReadEntity',
    'BfReadAngle',
    'BfReadCoord',
    'BfReadVecCoord',
    'BfReadVecNormal',
    'BfReadAngles',
    'BfGetNumBytesLeft',
    'CreateProfiler',
    'StartProfiling',
    'StopProfiling',
    'GetProfilerTime',
    'OnPluginStart',
    'AskPluginLoad2',
    'OnPluginEnd',
    'OnPluginPauseChange',
    'OnGameFrame',
    'OnMapStart',
    'OnMapEnd',
    'OnConfigsExecuted',
    'OnAutoConfigsBuffered',
    'OnAllPluginsLoaded',
    'GetMyHandle',
    'GetPluginIterator',
    'MorePlugins',
    'ReadPlugin',
    'GetPluginStatus',
    'GetPluginFilename',
    'IsPluginDebugging',
    'GetPluginInfo',
    'FindPluginByNumber',
    'SetFailState',
    'ThrowError',
    'GetTime',
    'FormatTime',
    'LoadGameConfigFile',
    'GameConfGetOffset',
    'GameConfGetKeyValue',
    'GameConfGetAddress',
    'GetSysTickCount',
    'AutoExecConfig',
    'RegPluginLibrary',
    'LibraryExists',
    'GetExtensionFileStatus',
    'OnLibraryAdded',
    'OnLibraryRemoved',
    'ReadMapList',
    'SetMapListCompatBind',
    'OnClientFloodCheck',
    'OnClientFloodResult',
    'CanTestFeatures',
    'GetFeatureStatus',
    'RequireFeature',
    'LoadFromAddress',
    'StoreToAddress',
    'CreateStack',
    'PushStackCell',
    'PushStackString',
    'PushStackArray',
    'PopStackCell',
    'PopStackString',
    'PopStackArray',
    'IsStackEmpty',
    'PopStack',
    'OnPlayerRunCmd',
    'BuildPath',
    'OpenDirectory',
    'ReadDirEntry',
    'OpenFile',
    'DeleteFile',
    'ReadFileLine',
    'ReadFile',
    'ReadFileString',
    'WriteFile',
    'WriteFileString',
    'WriteFileLine',
    'ReadFileCell',
    'WriteFileCell',
    'IsEndOfFile',
    'FileSeek',
    'FilePosition',
    'FileExists',
    'RenameFile',
    'DirExists',
    'FileSize',
    'FlushFile',
    'RemoveDir',
    'CreateDirectory',
    'GetFileTime',
    'LogToOpenFile',
    'LogToOpenFileEx',
    'PbReadInt',
    'PbReadFloat',
    'PbReadBool',
    'PbReadString',
    'PbReadColor',
    'PbReadAngle',
    'PbReadVector',
    'PbReadVector2D',
    'PbGetRepeatedFieldCount',
    'PbSetInt',
    'PbSetFloat',
    'PbSetBool',
    'PbSetString',
    'PbSetColor',
    'PbSetAngle',
    'PbSetVector',
    'PbSetVector2D',
    'PbAddInt',
    'PbAddFloat',
    'PbAddBool',
    'PbAddString',
    'PbAddColor',
    'PbAddAngle',
    'PbAddVector',
    'PbAddVector2D',
    'PbRemoveRepeatedFieldValue',
    'PbReadMessage',
    'PbReadRepeatedMessage',
    'PbAddMessage',
    'SetNextMap',
    'GetNextMap',
    'ForceChangeLevel',
    'GetMapHistorySize',
    'GetMapHistory',
    'GeoipCode2',
    'GeoipCode3',
    'GeoipCountry',
    'MarkNativeAsOptional',
    'RegClientCookie',
    'FindClientCookie',
    'SetClientCookie',
    'GetClientCookie',
    'SetAuthIdCookie',
    'AreClientCookiesCached',
    'OnClientCookiesCached',
    'CookieMenuHandler',
    'SetCookiePrefabMenu',
    'SetCookieMenuItem',
    'ShowCookieMenu',
    'GetCookieIterator',
    'ReadCookieIterator',
    'GetCookieAccess',
    'GetClientCookieTime',
    'LoadTranslations',
    'SetGlobalTransTarget',
    'GetClientLanguage',
    'GetServerLanguage',
    'GetLanguageCount',
    'GetLanguageInfo',
    'SetClientLanguage',
    'GetLanguageByCode',
    'GetLanguageByName',
    'CS_OnBuyCommand',
    'CS_OnCSWeaponDrop',
    'CS_OnGetWeaponPrice',
    'CS_OnTerminateRound',
    'CS_RespawnPlayer',
    'CS_SwitchTeam',
    'CS_DropWeapon',
    'CS_TerminateRound',
    'CS_GetTranslatedWeaponAlias',
    'CS_GetWeaponPrice',
    'CS_GetClientClanTag',
    'CS_SetClientClanTag',
    'CS_GetTeamScore',
    'CS_SetTeamScore',
    'CS_GetMVPCount',
    'CS_SetMVPCount',
    'CS_GetClientContributionScore',
    'CS_SetClientContributionScore',
    'CS_GetClientAssists',
    'CS_SetClientAssists',
    'CS_AliasToWeaponID',
    'CS_WeaponIDToAlias',
    'CS_IsValidWeaponID',
    'CS_UpdateClientModel',
    'LogToGame',
    'SetRandomSeed',
    'GetRandomFloat',
    'GetRandomInt',
    'IsMapValid',
    'IsDedicatedServer',
    'GetEngineTime',
    'GetGameTime',
    'GetGameTickCount',
    'GetGameDescription',
    'GetGameFolderName',
    'GetCurrentMap',
    'PrecacheModel',
    'PrecacheSentenceFile',
    'PrecacheDecal',
    'PrecacheGeneric',
    'IsModelPrecached',
    'IsDecalPrecached',
    'IsGenericPrecached',
    'PrecacheSound',
    'IsSoundPrecached',
    'CreateDialog',
    'GetEngineVersion',
    'PrintToChat',
    'PrintToChatAll',
    'PrintCenterText',
    'PrintCenterTextAll',
    'PrintHintText',
    'PrintHintTextToAll',
    'ShowVGUIPanel',
    'CreateHudSynchronizer',
    'SetHudTextParams',
    'SetHudTextParamsEx',
    'ShowSyncHudText',
    'ClearSyncHud',
    'ShowHudText',
    'ShowMOTDPanel',
    'DisplayAskConnectBox',
    'EntIndexToEntRef',
    'EntRefToEntIndex',
    'MakeCompatEntRef',
    'SetClientViewEntity',
    'SetLightStyle',
    'GetClientEyePosition',
    'CreateDataPack',
    'WritePackCell',
    'WritePackFloat',
    'WritePackString',
    'ReadPackCell',
    'ReadPackFloat',
    'ReadPackString',
    'ResetPack',
    'GetPackPosition',
    'SetPackPosition',
    'IsPackReadable',
    'LogMessage',
    'LogToFile',
    'LogToFileEx',
    'LogAction',
    'LogError',
    'OnLogAction',
    'GameLogHook',
    'AddGameLogHook',
    'RemoveGameLogHook',
    'FindTeamByName',
    'StartPrepSDKCall',
    'PrepSDKCall_SetVirtual',
    'PrepSDKCall_SetSignature',
    'PrepSDKCall_SetAddress',
    'PrepSDKCall_SetFromConf',
    'PrepSDKCall_SetReturnInfo',
    'PrepSDKCall_AddParameter',
    'EndPrepSDKCall',
    'SDKCall',
    'GetPlayerResourceEntity',
)


if __name__ == '__main__':  # pragma: no cover
    import re
    from urllib.request import FancyURLopener

    from pygments.util import format_lines

    class Opener(FancyURLopener):
        version = 'Mozilla/5.0 (Pygments Sourcemod Builtins Update)'

    opener = Opener()

    def get_version():
        f = opener.open('http://docs.sourcemod.net/api/index.php')
        r = re.compile(r'SourceMod v\.<b>([\d\.]+(?:-\w+)?)</td>')
        for line in f:
            m = r.search(line.decode())
            if m is not None:
                return m.groups()[0]
        raise ValueError('No version in api docs')

    def get_sm_functions():
        f = opener.open('http://docs.sourcemod.net/api/SMfuncs.js')
        r = re.compile(r'SMfunctions\[\d+\] = Array \("(?:public )?([^,]+)",".+"\);')
        functions = []
        for line in f:
            m = r.match(line.decode())
            if m is not None:
                functions.append(m.groups()[0])
        return functions

    def regenerate(filename, natives):
        with open(filename, encoding='utf-8') as fp:
            content = fp.read()

        header = content[:content.find('FUNCTIONS = (')]
        footer = content[content.find("if __name__ == '__main__':")-1:]


        with open(filename, 'w', encoding='utf-8') as fp:
            fp.write(header)
            fp.write(format_lines('FUNCTIONS', natives))
            fp.write('\n\n' + footer)

    def run():
        version = get_version()
        print('> Downloading function index for SourceMod %s' % version)
        functions = get_sm_functions()
        print('> %d functions found:' % len(functions))

        functionlist = []
        for full_function_name in functions:
            print('>> %s' % full_function_name)
            functionlist.append(full_function_name)

        regenerate(__file__, functionlist)


    run()
