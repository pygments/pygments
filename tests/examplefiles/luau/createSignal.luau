--[[
	Copyright (c) 2023 Roblox Corporation

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
]]

--!strict
local types = require(script.Parent.types)

type Callback<T> = types.Callback<T>
type Subscription = types.Subscription
type Signal<T> = types.Signal<T>
type FireSignal<T> = types.FireSignal<T>

type InternalSubscription<T> = { callback: Callback<T>, unsubscribed: boolean }

local function createSignal<T>(): (Signal<T>, FireSignal<T>)
	local subscriptions: { [Callback<T>]: InternalSubscription<T> } = {}
	local suspendedSubscriptions = {}

	local firing = false

	local function subscribe(_self: Signal<T>, callback)
		local subscription = {
			callback = callback,
			unsubscribed = false,
		}

		-- If the callback is already registered, don't add to the
		-- suspendedConnection. Otherwise, this will disable the existing one.
		if firing and not subscriptions[callback] then
			suspendedSubscriptions[callback] = subscription
		else
			subscriptions[callback] = subscription
		end

		local function unsubscribe(_self: Subscription)
			subscription.unsubscribed = true
			subscriptions[callback] = nil
			suspendedSubscriptions[callback] = nil
		end

		return {
			unsubscribe = unsubscribe,
		}
	end

	local function fire(value: T)
		firing = true
		for callback, subscription in subscriptions do
			if not subscription.unsubscribed and not suspendedSubscriptions[callback] then
				callback(value)
			end
		end

		firing = false
		for callback, subscription in suspendedSubscriptions do
			subscriptions[callback] = subscription
		end
		table.clear(suspendedSubscriptions)
	end

	return {
		subscribe = subscribe,
	}, fire
end

return createSignal