import DS from 'ember-data';
import Ember from 'ember';

export default DS.RESTSerializer.extend(DS.EmbeddedRecordsMixin, {
    // Specify embedded attributes
    attrs: {
        // Bucket Type properties and Bucket properties
        props: {
            embedded: 'always'
        }
    },

    /**
    This indicates that the
        store should call `normalizeResponse` instead of `extract` and to expect
        a JSON-API Document back.
    @property isNewSerializerAPI
    */
    isNewSerializerAPI: false,

    /**
     `keyForAttribute` can be used to define rules for how to convert an
     attribute name in your model to a key in your JSON.

     @method keyForAttribute
     @param {String} key
     @param {String} method
     @return {String} normalized key
    */
    keyForAttribute: function(attr /*, method*/) {
        // Riak and Explorer json uses snake case, like 'development_mode'
        return Ember.String.underscore(attr);
    },
    /**
    This method is used to convert each JSON root key in the payload
    into a modelName that it can use to look up the appropriate model for
    that part of the payload.
    @method modelNameFromPayloadKey
    @param {String} key
    @return {String} the model's modelName
    */
    modelNameFromPayloadKey: function(payloadKey) {
        return this._super(payloadKey);
    }

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
