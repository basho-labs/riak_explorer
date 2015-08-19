import DS from 'ember-data';
import Ember from 'ember';
import objectToArray from '../utils/riak-util';

/**
Unifies the Bucket Type properties and Bucket Properties
*/
var BucketProps = DS.Model.extend({
    // Raw object from JSON payload
    // {"allow_mult":false, "basic_quorum":false, ... }
    props: DS.attr(),

    propsList: function() {
        if(!this.get('props')) {
            return [];
        }
        return objectToArray(this.get('props'));
    }.property('props'),

    // Helper functions to access Properties

    // Siblings enabled
    allowMult: function() {
        return this.get('props').allow_mult;
    }.property('props'),

    dataTypeName: function() {
        var name;
        if(this.get('isCRDT')) {
            name = this.get('props').datatype;
        }
        if(name) {
            return name.capitalize();
        }
    }.property('props'),

    // Pre-commit or post-commit hooks enabled
    hasCommitHooks: function() {
        var hasPrecommit = !Ember.isEmpty(this.get('props').precommit);
        var hasPostcommit = !Ember.isEmpty(this.get('props').postcommit);
        if(hasPrecommit || hasPostcommit) {
            return true;
        }
        return false;
    }.property('props'),

    // Bucket Type activated via riak-admin command line
    isActive: function() {
        return this.get('props').active;
    }.property('props'),

    isCounter: function() {
        return this.get('dataTypeName') === 'Counter';
    }.property('props'),

    isCRDT: function() {
        return this.get('props').datatype;
    }.property('props'),

    // Last Write Wins optimization
    isLWW: function() {
        return this.get('props').last_write_wins;
    }.property('props'),

    isMap: function() {
        return this.get('dataTypeName') === 'Map';
    }.property('props'),

    // Has a Riak Search index been associated with this bucket type
    isSearchIndexed: function() {
        return this.get('searchIndexName');
    }.property('props'),

    isSet: function() {
        return this.get('dataTypeName') === 'Set';
    }.property('props'),

    isStronglyConsistent: function() {
        return false;
    }.property('props'),

    // Riak 2.1+ feature
    isWriteOnce: function() {
        return this.get('props').write_once;
    }.property('props'),

    nVal: function() {
        return this.get('props').n_val;
    }.property('props'),

    // What conflict resolution strategy this bucket type uses
    resolutionStrategy: function() {
        if(this.get('isStronglyConsistent')) {
            return 'Strongly Consistent';
        }
        // if(this.get('isCRDT')) {
            if(this.get('isCounter')) {
                return 'Convergent, Pairwise Maximum Wins';
            }
            if(this.get('isMap')) {
                return 'Convergent, Add/Update Wins Over Remove';
            }
            if(this.get('isSet')) {
                return 'Convergent, Add Wins Over Remove';
            }
        // }
        if(this.get('allowMult')) {
            return 'Causal Context (Siblings Enabled)';
        }
        if(this.get('isWriteOnce')) {
            return 'n/a (Write-Once Optimized)';
        }
        // Last Write Wins optimization enabled
        if(this.get('isLWW')) {
            return 'Wall Clock (LastWriteWins enabled)';
        }

        // Default, regular riak object, allow_mult = false
        return 'Causal Context (Siblings Off, fallback to Wall Clock)';
    }.property('props'),

    // What type of objects are stored (default, search indexed, CRDTs)
    objectType: function() {
        var type = [];
        if(this.get('isCRDT')) {
            type.push(this.get('dataTypeName'));
        } else {
            type.push('Default');
        }
        if(this.get('isSearchIndexed')) {
            type.push('Search Indexed');
        }
        return type.join(', ');
    }.property('props'),

    quorum: function() {
        return {
            r: this.get('props').r,    // Read quorum
            w: this.get('props').r,    // Write Quorum
            pr: this.get('props').pr,  // Primary Read
            pw: this.get('props').pw,  // Primary Write
            dw: this.get('props').dw,  // Durable Write
            basic_quorum: this.get('props').basic_quorum,
            notfound_ok: this.get('props').notfound_ok
        };
    }.property('props'),

    // Whether or not the notion of Eventual Consistency / Quorums applies
    // (meaning, if it's not Strongly consistent)
    quorumRelevant: function() {
        return !this.get('isStronglyConsistent');
    }.property('props'),

    searchIndexName: function() {
        return this.get('props').search_index;
    }.property('props'),

    warnings: function() {
        var warnings = [];
        if(this.get('isStronglyConsistent')) {
            if(this.get('nVal') < 5) {
                warnings.push('Using Strong Consistency, but n_val < 5!');
            }
            if(this.get('isSearchIndexed')) {
                warnings.push('Combining Strong Consistency with Search. Use cation!');
            }
            if(this.get('hasCommitHooks')) {
                warnings.push('Using commit hooks, but those are ignored for Strongly Consistent data!');
            }
        }
        if(this.get('allowMult')) {  // Siblings enabled
            if(!this.get('props').dvv_enabled) {
                warnings.push('Dotted Version Vectors (dvv_enabled) should be enabled when Siblings are enabled.');
            }
        }
        return warnings;
    }.property('props')
});

export default BucketProps;
