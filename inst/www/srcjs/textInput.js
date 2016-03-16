var rtTextInputBinding = new Shiny.InputBinding();
$.extend(rtTextInputBinding, {
    find: function (scope) {
        return $(scope).find('.text-input');
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
        $(el).on('keyup.rtTextInputBinding input.rtTextInputBinding', function (event) {
            callback(true);
        });
        $(el).on('change.rtTextInputBinding', function (event) {
            callback(false);
        });
    },
    unsubscribe: function (el) {
        $(el).off('.rtTextInputBinding');
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
Shiny.inputBindings.register(rtTextInputBinding);
