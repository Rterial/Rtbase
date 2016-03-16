var rtTextAreaInputBinding = new Shiny.InputBinding();
$.extend(rtTextAreaInputBinding, {
    find: function (scope) {
        return $(scope).find('.materialize-textarea');
    },
    getId: function (el) {
        return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function (el) {
        return el.value;
    },
    setValue: function (el, value) {
        el.value = value;
    },
    subscribe: function (el, callback) {
        $(el).on('keyup.rtTextAreaInputBinding input.rtTextAreaInputBinding', function (event) {
            callback(true);
        });
        $(el).on('change.rtTextAreaInputBinding', function (event) {
            callback(false);
        });
    },
    unsubscribe: function (el) {
        $(el).off('.rtTextAreaInputBinding');
    },
    receiveMessage: function (el, data) {
        if (data.hasOwnProperty('value'))
            this.setValue(el, data.value);

        if (data.hasOwnProperty('label'))
            $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

        $(el).trigger('change');
    },
    getState: function (el) {
        return {
            label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
            value: el.value
        };
    },
    getRatePolicy: function () {
        return {
            policy: 'debounce',
            delay: 250
        };
    }
});
Shiny.inputBindings.register(rtTextAreaInputBinding);
