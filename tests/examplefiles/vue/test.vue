<script>
export default {
    data() {
        return {
            greeting: 'Hello World!'
        }
    }
}
</script>

<template>
    <p class="greeting">{{ greeting }}</p>
    <div v-bind="objectOfAttrs"></div>
    {{ number + 1 }}
    {{ ok ? 'YES' : 'NO' }}
    {{ message.split('').reverse().join('') }}
    <div :id="`list-${id}`"></div>
    <span :title="toTitleDate(date)">
        {{ formatDate(date) }}
    </span>
    <p v-if="seen">Now you see me</p>
    <a v-bind:href="url"> ... </a>
    <!-- shorthand -->
    <a :href="url"> ... </a>
    <a v-on:click="doSomething"> ... </a>
    <!-- shorthand -->
    <a @click="doSomething"> ... </a>
    <a v-bind:[attributeName]="url"> ... </a>
    <a :[attributeName]="url"> ... </a>
    <a v-on:[eventName]="doSomething"> ... </a>
    <a @[eventName]="doSomething"></a>
    <form @submit.prevent="onSubmit">...</form>
    <li v-for="item in items">
        {{ item.message }}
    </li>
    <li v-for="(item, index) in items">
        {{ parentMessage }} - {{ index }} - {{ item.message }}
    </li>
    <li v-for="{ message } in items">
        {{ message }}
    </li>
    <li v-for="({ message }, index) in items">
        {{ message }} {{ index }}
    </li>
    <li v-for="item in items">
        <span v-for="childItem in item.children">
            {{ item.message }} {{ childItem }}
        </span>
    </li>
    <div v-for="item of items"></div>
</template>

<style>
.greeting {
    color: red;
    font-weight: bold;
}
</style>