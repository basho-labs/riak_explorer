import DS from 'ember-data';
import Ember from 'ember';

export default DS.RESTSerializer.extend({
    /**
    This indicates that the
        store should call `normalizeResponse` instead of `extract` and to expect
        a JSON-API Document back.
    @property isNewSerializerAPI
    */
    isNewSerializerAPI: true,

    /**
    Normalizes a part of the JSON payload returned by
    the server.
    @method normalize
    @param {DS.Model} typeClass
    @param {Object} hash
    @return {Object}
    */
    // normalize: function(typeClass, hash) {
    //   var fields = Ember.get(typeClass, 'fields');
    //   fields.forEach(function(field) {
    //     var payloadField = Ember.String.underscore(field);
    //     if (field === payloadField) { return; }
    //
    //     hash[field] = hash[payloadField];
    //     delete hash[payloadField];
    //   });
    //   return this._super.apply(this, arguments);
    // }
    // keyForAttribute: function(attr, method) {
    //        return Ember.String.underscore(attr);
    // }
});
